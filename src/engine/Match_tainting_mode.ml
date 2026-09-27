(* Iago Abal, Yoann Padioleau
 *
 * Copyright (C) 2019-2024 Semgrep Inc.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file LICENSE.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * LICENSE for more details.
 *)
open Common
open Fpath_.Operators
module D = Dataflow_tainting
module Var_env = Dataflow_var_env
module G = AST_generic
module H = AST_generic_helpers
module R = Rule
module PM = Core_match
module RP = Core_result
module T = Taint
module Lval_env = Taint_lval_env
module MV = Metavariable
module ME = Matching_explanation
module OutJ = Semgrep_output_v1_t
module Labels = Set.Make (String)

module Log = Log_tainting.Log
module Effect = Shape_and_sig.Effect
module Effects = Shape_and_sig.Effects
module Signature = Shape_and_sig.Signature

type fun_info = {
  name : IL.name;
  class_name_str : string option;
  method_properties : AST_generic.expr list;
  cfg : IL.fun_cfg;
  fdef : G.function_definition;
  is_static : bool;  (* [@staticmethod] and the like: no implicit receiver *)
  is_lambda_assignment : bool;
  taint_inst : Taint_rule_inst.t option;  (* [Some] cross-file preds, else current-file *)
}

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Wrapper around the tainting dataflow-based analysis. *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
module F2 = IL

module DataflowY = Dataflow_core.Make (struct
  type node = F2.node
  type edge = F2.edge
  type flow = (node, edge) CFG.t

  let short_string_of_node n = Display_IL.short_string_of_node_kind n.F2.n
end)

let get_source_requires src =
  let _pm, src_spec = T.pm_of_trace src.T.call_trace in
  src_spec.R.source_requires

(*****************************************************************************)
(* Testing whether some matches a taint spec *)
(*****************************************************************************)

let lazy_force x = Lazy.force x [@@profiling]

(*****************************************************************************)
(* Pattern match from finding *)
(*****************************************************************************)

(* If the 'requires' has the shape 'A and ...' then we assume that 'A' is the
 * preferred label for reporting the taint trace. *)
let preferred_label_of_sink ({ rule_sink; _ } : Effect.sink) =
  match rule_sink.sink_requires with
  | Some { precondition = PAnd (PLabel label :: _); _ } -> Some label
  | Some _
  | None ->
      None

let rec convert_taint_call_trace = function
  | Taint.Flat_PM (pm, _) ->
      let toks = Lazy.force pm.tokens |> List.filter Tok.is_origintok in
      Taint_trace.Toks toks
  | Taint.Flat_call { callee; tokens; inner; _ } ->
      Taint_trace.Call
        {
          call_toks =
            AST_generic_helpers.ii_of_any (G.E callee)
            |> List.filter Tok.is_origintok;
          intermediate_vars = tokens;
          call_trace = convert_taint_call_trace inner;
        }

let rec leaf_tokens (trace : Taint_trace.call_trace) : Tok.t list =
  match trace with
  | Taint_trace.Toks toks -> toks
  | Taint_trace.Call { call_trace; _ } -> leaf_tokens call_trace

(* Carried guards ([Sig_inst.classify_guards] defers every non-dispatch
 * guard) are decided here, when the effect becomes a match: the effect-level
 * guard drops the whole effect if it folds to false, and each sink item's
 * guard — the guard its taint carried when it reached the sink — drops that
 * item. This runs before any match deduplication ([PM.uniq] and reporting's
 * dedup_and_sort), so a finding survives iff some candidate's guard is not
 * false. An undecided guard reports, as a guard-less effect would. *)
let guard_folds_false (g : Effect_guard.t) : bool =
  (not (Effect_guard.is_top g))
  &&
  match Effect_guard.eval g.cond with
  | Some false -> true
  | Some true
  | None ->
      false

module PF = Path_feasibility

module Cfg_tbl = Hashtbl.Make (struct
  type t = IL.fun_cfg

  let equal (cfg1 : t) (cfg2 : t) : bool = phys_equal cfg1 cfg2
  let hash (cfg : t) : int = Hashtbl.hash cfg.IL.cfg.graph
end)

module Fid_tbl = Hashtbl.Make (Function_id)

type retention = {
  lang : Lang.t;
  cfg_of : Function_id.t -> IL.fun_cfg option;
  index_of : IL.fun_cfg -> PF.index;
  check :
    IL.fun_cfg -> entry:PF.state -> PF.anchor list -> PF.verdict * PF.state option list;
  reanalysed :
    IL.fun_cfg ->
    Shape_and_sig.signature_database option ->
    (Shape_and_sig.signature_database option -> Effects.t) ->
    Effects.t;
  retained_db :
    Function_id.t list -> Shape_and_sig.signature_database option;
  retain_tables : Taint_shared_tables.t;
}

type checked_function = {
  retention : retention;
  cfg : IL.fun_cfg;
  reanalyse : (Shape_and_sig.signature_database option -> Effects.t) option;
}

let retaining (taint_inst : Taint_rule_inst.t) : Taint_rule_inst.t =
  { taint_inst with merge = T.Keep_both }

let mk_retention ~(lang : Lang.t) ~(cfg_of : Function_id.t -> IL.fun_cfg option)
    ~(shared_tables : Taint_shared_tables.t)
    ~(retain_signature :
       Taint_shared_tables.t ->
       Function_id.t ->
       Shape_and_sig.signature_database ->
       Shape_and_sig.signature_database)
    (db : Shape_and_sig.signature_database option) : retention =
  let indexes : PF.index Cfg_tbl.t = Cfg_tbl.create 16 in
  let retained : unit Fid_tbl.t = Fid_tbl.create 16 in
  let retain_tables =
    {
      shared_tables with
      Taint_shared_tables.constructor_envs =
        Hashtbl.copy shared_tables.Taint_shared_tables.constructor_envs;
    }
  in
  let current = ref db in
  let index_of (cfg : IL.fun_cfg) : PF.index =
    match Cfg_tbl.find_opt indexes cfg with
    | Some ix -> ix
    | None ->
        let ix = PF.index cfg in
        Cfg_tbl.add indexes cfg ix;
        ix
  in
  let checks :
      ( int,
        (IL.fun_cfg
        * PF.state
        * PF.anchor list
        * (PF.verdict * PF.state option list))
        list )
      Hashtbl.t =
    Hashtbl.create 64
  in
  let reanalyses :
      (Shape_and_sig.signature_database option * Effects.t) list Cfg_tbl.t =
    Cfg_tbl.create 16
  in
  {
    lang;
    cfg_of;
    index_of;
    reanalysed =
      (fun (cfg : IL.fun_cfg) (db : Shape_and_sig.signature_database option)
           (reanalyse : Shape_and_sig.signature_database option -> Effects.t) ->
        let computed = Option.value (Cfg_tbl.find_opt reanalyses cfg) ~default:[] in
        match
          List.find_opt
            (fun ((db', _) : Shape_and_sig.signature_database option * Effects.t) ->
              Option.equal Common.phys_equal db' db)
            computed
        with
        | Some (_, effects) -> effects
        | None ->
            let effects = reanalyse db in
            Cfg_tbl.replace reanalyses cfg ((db, effects) :: computed);
            effects);
    check =
      (fun (cfg : IL.fun_cfg) ~(entry : PF.state) (anchors : PF.anchor list) ->
        let key = PF.hash_anchors anchors in
        let checked = Option.value (Hashtbl.find_opt checks key) ~default:[] in
        match
          List.find_opt
            (fun ((cfg', entry', anchors', _) :
                   IL.fun_cfg
                   * PF.state
                   * PF.anchor list
                   * (PF.verdict * PF.state option list)) ->
              Common.phys_equal cfg' cfg
              && List.equal PF.equal_anchor anchors' anchors
              && PF.equal_state entry' entry)
            checked
        with
        | Some (_, _, _, result) -> result
        | None ->
            let result = PF.check lang cfg (index_of cfg) ~entry anchors in
            Hashtbl.replace checks key ((cfg, entry, anchors, result) :: checked);
            result);
    retained_db =
      (fun (fids : Function_id.t list) ->
        List.iter
          (fun (fid : Function_id.t) ->
            if not (Fid_tbl.mem retained fid) then (
              Fid_tbl.add retained fid ();
              current :=
                Option.map (retain_signature retain_tables fid) !current))
          fids;
        !current);
    retain_tables;
  }

type candidate = {
  source : T.source;
  tokens : T.tainted_tokens;
  steps : T.step list;
  source_trace : R.taint_source T.flat_call_trace;
  sink_trace : unit T.flat_call_trace;
  refuted_by_guard : bool;
}

let candidates_of_item ~valid (item : Effect.taint_to_sink_item) :
    candidate Seq.t =
  T.taint_resolutions ~valid item.taint
  |> Seq.flat_map (fun (r : T.resolved) ->
         match Option.value r.resolved_orig ~default:item.taint.orig with
         | Src src ->
             let sink_trace =
               Option.value r.resolved_sink_trace ~default:item.sink_trace
             in
             T.source_trace_resolutions ~valid src.call_trace
             |> Seq.flat_map
                  (fun ((source_trace, source_refuted) :
                         R.taint_source T.flat_call_trace * bool) ->
                    T.sink_trace_resolutions ~valid sink_trace
                    |> Seq.map
                         (fun ((sink_trace, sink_refuted) :
                                unit T.flat_call_trace * bool) ->
                           {
                             source = src;
                             tokens = r.resolved_tokens;
                             steps = r.resolved_steps;
                             source_trace;
                             sink_trace;
                             refuted_by_guard =
                               r.refuted_by_guard || source_refuted
                               || sink_refuted;
                           }))
         (* even if there is any taint "variable", it's irrelevant for the
          * finding, since the precondition is satisfied. *)
         | Var _
         | Shape_var _
         | Control ->
             Seq.empty)

let trace_of_candidate (c : candidate) : Taint_trace.item =
  {
    Taint_trace.source_trace = convert_taint_call_trace c.source_trace;
    tokens = c.tokens;
    sink_trace = convert_taint_call_trace c.sink_trace;
  }

let origin_of_candidate (c : candidate) : Tok.t list =
  leaf_tokens (convert_taint_call_trace c.source_trace)

(* For now CLI does not support multiple taint traces for a finding, and it
 * simply picks the _first_ trace from this list. So here we apply a number
 * of heuristics to make sure the first trace in this list is the most
 * relevant one. This is particularly important when using (experimental)
 * taint labels, because not all labels are equally relevant for the finding. *)
let sources_of_taints ~valid ?preferred_label
    (taints : Effect.taint_to_sink_item list) :
    (candidate * Effect.taint_to_sink_item) list =
  (* We only report actual sources reaching a sink. If users want Semgrep to
   * report function parameters reaching a sink without sanitization, then
   * they need to specify the parameters as taint sources. *)
  let taint_sources =
    taints
    |> List_.filter_map (fun (item : Effect.taint_to_sink_item) ->
           match candidates_of_item ~valid item () with
           | Seq.Cons (c, _) -> Some (c, item)
           | Seq.Nil -> None)
  in
  let taint_sources =
    (* If there is a "preferred label", then sort sources to make sure this
       label is picked before others. See 'preferred_label_of_sink'. *)
    match preferred_label with
    | None -> taint_sources
    | Some label ->
        taint_sources
        |> List.stable_sort
             (fun
               ((c1, _) : candidate * Effect.taint_to_sink_item)
               ((c2, _) : candidate * Effect.taint_to_sink_item)
             ->
               match
                 (String.equal c1.source.label label, String.equal c2.source.label label)
               with
               | true, false -> -1
               | false, true -> 1
               | false, false
               | true, true ->
                   0)
  in
  (* We prioritize taint sources without preconditions,
     selecting their traces first, and then consider sources
     with preconditions as a secondary choice. *)
  let with_req, without_req =
    taint_sources
    |> Either_.partition
         (fun ((c, _) as source : candidate * Effect.taint_to_sink_item) ->
           match get_source_requires c.source with
           | Some _ -> Left source
           | None -> Right source)
  in
  if not (List_.null without_req) then without_req
  else (
    Log.warn (fun m ->
        m
          "Taint source without precondition wasn't found. Displaying the \
           taint trace from the source with precondition.");
    with_req)

type activation_check = { anchors : PF.anchor list; children : (int * callee_check) list }
and callee_check = { site : T.call_site; check : activation_check }

let activation_check_of_parts (parts : (PF.anchor * callee_check option) list) : activation_check =
  {
    anchors = List.map fst parts;
    children =
      parts
      |> List.mapi (fun (i : int) ((_, child) : PF.anchor * callee_check option) ->
             Option.map (fun (c : callee_check) -> (i, c)) child)
      |> List.filter_map Fun.id;
  }

let rec parts_of_steps (steps : T.step list) :
    (PF.anchor * callee_check option) list =
  steps
  |> List.map (fun (step : T.step) ->
         match step with
         | T.Token tok -> (PF.Token tok, None)
         | T.Callee { site; steps; _ } ->
             ( PF.Call site.callee_exp,
               Some
                 {
                   site;
                   check =
                     activation_check_of_parts
                       (((PF.Entry, None) :: parts_of_steps steps)
                       @ [ (PF.Exit, None) ]);
                 } ))

let rec sink_part (trace : unit T.flat_call_trace) :
    PF.anchor * callee_check option =
  match trace with
  | Flat_PM (pm, _) -> (PF.Range (fst pm.range_loc, snd pm.range_loc), None)
  | Flat_call { site; steps; inner; _ } ->
      ( PF.Call site.callee_exp,
        Some
          {
            site;
            check =
              activation_check_of_parts
                (((PF.Entry, None) :: parts_of_steps steps) @ [ sink_part inner ]);
          } )

let rec source_part (trace : R.taint_source T.flat_call_trace) :
    PF.anchor * callee_check option =
  match trace with
  | Flat_PM (pm, _) -> (PF.Range (fst pm.range_loc, snd pm.range_loc), None)
  | Flat_call { site; steps; inner; _ } ->
      ( PF.Call site.callee_exp,
        Some
          {
            site;
            check =
              activation_check_of_parts
                ((PF.Entry, None) :: source_part inner
                 :: (parts_of_steps steps @ [ (PF.Exit, None) ]));
          } )

let activation_check_of_candidate (c : candidate) : activation_check =
  activation_check_of_parts
    ((PF.Entry, None) :: source_part c.source_trace
     :: (parts_of_steps c.steps @ [ sink_part c.sink_trace ]))

let conjoin_verdicts (v1 : PF.verdict) (v2 : PF.verdict) : PF.verdict =
  match (v1, v2) with
  | Infeasible, _
  | _, Infeasible ->
      Infeasible
  | Feasible, Feasible -> Feasible
  | Unknown, _
  | _, Unknown ->
      Unknown

let rec verify_activation (retention : retention) (cfg : IL.fun_cfg) (entry : PF.state)
    (fc : activation_check) : PF.verdict * PF.state option =
  let verdict, states =
    retention.check cfg ~entry fc.anchors
  in
  let at (i : int) : PF.state option = Option.join (List.nth_opt states i) in
  let final = at (List.length states - 1) in
  let verdict =
    List.fold_left
      (fun (acc : PF.verdict) ((i, child) : int * callee_check) ->
        match acc with
        | Infeasible -> Infeasible
        | Feasible
        | Unknown ->
            conjoin_verdicts acc (verify_callee retention (at i) child))
      verdict fc.children
  in
  (verdict, final)

and verify_callee (retention : retention) (at_call : PF.state option)
    (child : callee_check) : PF.verdict =
  let params =
    List.filter_map IL_helpers.pname_of_param child.site.callee_params_il
  in
  let reads_parameters_only ((atom, _) : IL.exp * bool) : bool =
    IL_helpers.lvals_of_exp atom
    |> List.for_all (fun (lval : IL.lval) ->
           match lval.base with
           | IL.Var name -> List.exists (IL.equal_name name) params
           | IL.VarSpecial _
           | IL.Mem _ ->
               false)
  in
  match Option.bind child.site.callee_fid retention.cfg_of with
  | None -> Unknown
  | Some cfg -> (
      let bindings =
        match at_call with
        | None -> []
        | Some caller ->
            Sig_inst.actuals_of_params child.site
            |> List.map (fun ((param, actual) : IL.name * IL.exp) ->
                   (param, PF.value retention.lang caller actual))
      in
      let verdict, final =
        verify_activation retention cfg (PF.entry_state bindings) child.check
      in
      match (verdict, at_call, final) with
      | Infeasible, _, _ -> Infeasible
      | (Feasible | Unknown), Some caller, Some callee
        when PF.refutes caller
               (Sig_inst.literals_at_call_site child.site
                  (List.filter reads_parameters_only (PF.literals callee)))
        ->
          Infeasible
      | (Feasible | Unknown), _, _ -> verdict)

let verify_candidate (checked_function : checked_function) (c : candidate) : PF.verdict =
  if c.refuted_by_guard then Infeasible
  else
    fst
      (verify_activation checked_function.retention checked_function.cfg (PF.entry_state [])
         (activation_check_of_candidate c))

let rec fids_of_activation_check (depth : int) (fc : activation_check) :
    (Function_id.t * int) list =
  fc.children
  |> List.concat_map (fun ((_, child) : int * callee_check) ->
         (match child.site.callee_fid with
         | Some fid -> [ (fid, depth) ]
         | None -> [])
         @ fids_of_activation_check (depth + 1) child.check)

let callee_fids (cs : candidate list) : Function_id.t list =
  cs
  |> List.concat_map (fun (c : candidate) ->
         fids_of_activation_check 0 (activation_check_of_candidate c))
  |> List.stable_sort (fun ((_, d1) : _ * int) ((_, d2) : _ * int) ->
         Int.compare d2 d1)
  |> List.fold_left
       (fun (acc : Function_id.t list) ((fid, _) : Function_id.t * int) ->
         if List.exists (Function_id.equal fid) acc then acc else fid :: acc)
       []
  |> List.rev

let anchor_position (anchor : PF.anchor) : int =
  match anchor with
  | PF.Entry -> -1
  | PF.Exit -> max_int
  | PF.Token tok -> (
      match Tok.loc_of_tok tok with
      | Ok loc -> loc.pos.bytepos
      | Error _ -> -1)
  | PF.Range (first, _) -> first.pos.bytepos
  | PF.Call callee -> (
      match AST_generic_helpers.range_of_any_opt (IL.any_of_orig callee.eorig) with
      | Some (first, _) -> first.pos.bytepos
      | None -> -1)

let rec positions (fc : activation_check) : int list list =
  List.map anchor_position fc.anchors
  :: List.concat_map
       (fun ((_, child) : int * callee_check) -> positions child.check)
       fc.children

let candidates_in_order ~valid (item : Effect.taint_to_sink_item) :
    candidate list =
  candidates_of_item ~valid item
  |> List.of_seq
  |> List.map (fun (c : candidate) ->
         (positions (activation_check_of_candidate c), trace_of_candidate c, c))
  |> List.stable_sort
       (fun
         ((p1, t1, _) : int list list * Taint_trace.item * candidate)
         ((p2, t2, _) : int list list * Taint_trace.item * candidate)
       ->
         match List.compare (List.compare Int.compare) p1 p2 with
         | 0 -> Taint_trace.compare_item t1 t2
         | c -> c)
  |> List.fold_left
       (fun (acc : (Taint_trace.item * candidate) list)
            ((_, t, c) : int list list * Taint_trace.item * candidate) ->
         match acc with
         | (previous, _) :: _ when Taint_trace.equal_item previous t -> acc
         | _ -> (t, c) :: acc)
       []
  |> List.rev_map snd

let reported_items ~lang (effect_ : Effect.t) :
    (Effect.taint_to_sink_item list
    * (T.call_site list -> Effect_guard.t -> bool))
    option =
  match effect_ with
  | ToSink { taints_with_precondition = taints, _; _ } ->
      let valid = Sig_inst.guard_valid_under ~lang (Effect.guards_of effect_) in
      Some
        ( taints
          |> List.filter (fun (i : Effect.taint_to_sink_item) ->
                 not (guard_folds_false i.guard))
          |> List.filter (fun (i : Effect.taint_to_sink_item) ->
                 valid [] i.guard),
          valid )
  | ToLval _
  | ToReturn _
  | ToSinkInCall _ ->
      None

let retained_candidates (checked_function : checked_function) ~(sink : string) (effect_ : Effect.t)
    (items : Effect.taint_to_sink_item list) :
    (Effect.taint_to_sink_item * candidate list) list =
  let lang = checked_function.retention.lang in
  let rec retain (fids : Function_id.t list) =
    match checked_function.reanalyse with
    | None -> None
    | Some reanalyse -> (
        Log_tainting.Trace_log.debug (fun m ->
            m
              "Taint trace: the reporting function of the sink at %s runs \
               again, keeping every alternative trace, with %d callees \
               that keep them too"
              sink (List.length fids));
        match
          Effects.find_opt effect_
            (checked_function.retention.reanalysed checked_function.cfg
               (checked_function.retention.retained_db fids)
               reanalyse)
          |> Fun.flip Option.bind (reported_items ~lang)
        with
        | None -> None
        | Some (retained_items, valid) ->
            let found =
              retained_items
              |> List.map (fun (i : Effect.taint_to_sink_item) ->
                     (i, candidates_in_order ~valid i))
            in
            let more =
              found
              |> List.concat_map
                   (fun ((_, cs) : Effect.taint_to_sink_item * candidate list) ->
                     callee_fids cs)
              |> List.filter (fun (fid : Function_id.t) ->
                     not (List.exists (Function_id.equal fid) fids))
            in
            if List_.null more then Some found else retain (fids @ more))
  in
  let today (valid : T.call_site list -> Effect_guard.t -> bool) :
      (Effect.taint_to_sink_item * candidate list) list =
    items
    |> List.map (fun (i : Effect.taint_to_sink_item) ->
           (i, candidates_in_order ~valid i))
  in
  let valid = Sig_inst.guard_valid_under ~lang (Effect.guards_of effect_) in
  let first_fids =
    items
    |> List.concat_map (fun (i : Effect.taint_to_sink_item) ->
           match candidates_of_item ~valid i () with
           | Seq.Cons (c, _) -> callee_fids [ c ]
           | Seq.Nil -> [])
  in
  match retain first_fids with
  | None -> today valid
  | Some found ->
      items
      |> List.map (fun (i : Effect.taint_to_sink_item) ->
             match
               List.find_opt
                 (fun ((r, _) : Effect.taint_to_sink_item * candidate list) ->
                   Int.equal (T.compare_taint r.taint i.taint) 0)
                 found
             with
             | Some (_, cs) -> (i, cs)
             | None -> (i, candidates_in_order ~valid i))

let search (checked_function : checked_function) (candidates : candidate list) : candidate option =
  let rec go (unknown : candidate option) (cs : candidate list) =
    match cs with
    | [] -> unknown
    | c :: rest -> (
        match verify_candidate checked_function c with
        | Feasible -> Some c
        | Unknown -> go (first_some unknown (Some c)) rest
        | Infeasible -> go unknown rest)
  and first_some (a : candidate option) (b : candidate option) =
    match a with
    | Some _ -> a
    | None -> b
  in
  go None candidates

let displayed_trace (checked_function : checked_function option) (effect_ : Effect.t) (sink_pm : PM.t)
    (sources : (candidate * Effect.taint_to_sink_item) list) : Taint_trace.t =
  let sink = Tok.stringpos_of_tok (Tok.tok_of_loc (fst sink_pm.range_loc)) in
  let today =
    {
      Taint_trace.origin =
        (match sources with
        | (c, _) :: _ -> origin_of_candidate c
        | [] -> []);
      items =
        List_.map
          (fun ((c, _) : candidate * Effect.taint_to_sink_item) ->
            trace_of_candidate c)
          sources;
    }
  in
  match (checked_function, sources) with
  | None, _
  | _, [] ->
      today
  | Some checked_function, (first, _) :: _ -> (
      match verify_candidate checked_function first with
      | Feasible
      | Unknown ->
          today
      | Infeasible -> (
          Log_tainting.Trace_log.debug (fun m ->
              m
                "Taint trace: the trace to the sink at %s contradicts a branch \
                 condition or a guard"
                sink);
          let candidates =
            retained_candidates checked_function ~sink effect_ (List.map snd sources)
            |> List.concat_map snd
          in
          match search checked_function candidates with
          | Some c ->
              Log_tainting.Trace_log.debug (fun m ->
                  m
                    "Taint trace: the trace to the sink at %s is replaced, out \
                     of %d candidate traces"
                    sink (List.length candidates));
              {
                Taint_trace.origin = origin_of_candidate c;
                items = [ trace_of_candidate c ];
              }
          | None ->
              Log_tainting.Trace_log.debug (fun m ->
                  m
                    "Taint trace: every path of %d candidate traces to the sink \
                     at %s contradicts a branch condition; the finding is \
                     reported without a trace"
                    (List.length candidates) sink);
              { today with items = [] }))

let match_on_of_xconf (xconf : Match_env.xconfig) : [ `Sink | `Source ] =
  (* TEMPORARY HACK to support both taint_match_on (DEPRECATED) and
   * taint_focus_on (preferred name by SR). *)
  match (xconf.config.taint_focus_on, xconf.config.taint_match_on) with
  | `Source, _
  | _, `Source ->
      `Source
  | `Sink, `Sink -> `Sink

let pms_of_effect ~lang ~match_on ~(checked_function : checked_function option) (effect_ : Effect.t) =
  match effect_ with
  | ToLval _
  | ToReturn _
  | ToSinkInCall _ ->
      []
  | _ when guard_folds_false (Effect.guards_of effect_) -> []
  | ToSink
      {
        taints_with_precondition = _, requires;
        sink = { pm = sink_pm; _ } as sink;
        merged_env;
        _;
      } -> (
      match reported_items ~lang effect_ with
      | None -> []
      | Some (taints, valid) -> (
      let actual_taints = List_.map (fun t -> t.Effect.taint) taints in
      let satisfies =
        (not (List_.null taints))
        && T.taints_satisfy_requires actual_taints requires
      in
      if not satisfies then []
      else
        let preferred_label = preferred_label_of_sink sink in
        let taint_sources = sources_of_taints ~valid ?preferred_label taints in
        match match_on with
        | `Sink ->
            (* The old behavior used to be that, for sinks with a `requires`, we would
               generate a finding per every single taint source going in. Later deduplication
               would deal with it.
               We will instead choose to consolidate all sources into a single finding. We can
               do some postprocessing to report only relevant sources later on, but for now we
               will lazily (again) defer that computation to later.
            *)
            (* We always report the finding on the sink that gets tainted, the call trace
                * must be used to explain how exactly the taint gets there. At some point
                * we experimented with reporting the match on the `sink`'s function call that
                * leads to the actual sink. E.g.:
                *
                *     def f(x):
                *       sink(x)
                *
                *     def g():
                *       f(source)
                *
                * Here we tried reporting the match on `f(source)` as "the line to blame"
                * for the injection bug... but most users seem to be confused about this. They
                * already expect Semgrep (and DeepSemgrep) to report the match on `sink(x)`.
            *)
            let taint_trace =
              Some (lazy (displayed_trace checked_function effect_ sink_pm taint_sources))
            in
            [ { sink_pm with env = merged_env; taint_trace } ]
        | `Source ->
            taint_sources
            |> List_.map
                 (fun ((c, _) as source : candidate * Effect.taint_to_sink_item) ->
                   let src_pm, _ = T.pm_of_trace c.source.T.call_trace in
                   {
                     src_pm with
                     env = merged_env;
                     taint_trace =
                       Some
                         (lazy (displayed_trace checked_function effect_ sink_pm [ source ]));
                   })))

let pms_of_effects ~lang ~match_on ~(checked_function : checked_function option) (effects : Effects.t)
    : PM.t list =
  Effects.fold
    (fun (effect_ : Effect.t) (acc : PM.t list) ->
       List.rev_append (pms_of_effect ~lang ~match_on ~checked_function effect_) acc)
    effects []

let force_traces (matches : PM.t list) : PM.t list =
  List.iter
    (fun (pm : PM.t) ->
      match pm.taint_trace with
      | Some trace ->
          let (_ : Taint_trace.t) = Lazy.force trace in
          ()
      | None -> ())
    matches;
  matches

(*****************************************************************************)
(* Main entry points *)
(*****************************************************************************)

(* Analyse a function from a pre-built [IL.fun_cfg]. *)
let check_fundef_with_cfg (taint_inst : Taint_rule_inst.t)
    (shared_tables : Taint_shared_tables.t) (name : IL.name)
    ?glob_env ?class_name ?signature_db ?builtin_signature_db
    (fcfg : IL.fun_cfg) =
  let in_env, env_effects =
    Taint_input_env.mk_fun_input_env taint_inst shared_tables ?glob_env fcfg.IL.params
  in
  let effects, mapping =
    Dataflow_tainting.fixpoint taint_inst shared_tables ~in_env ~name ?class_name
      ?signature_db ?builtin_signature_db fcfg
  in
  let effects = Effects.union ~merge:taint_inst.merge env_effects effects in
  (fcfg, effects, mapping)

(* [check_fundef_with_cfg] on a freshly-lowered [fdef]. *)
let check_fundef (taint_inst : Taint_rule_inst.t)
    (shared_tables : Taint_shared_tables.t) (name : IL.name) ?glob_env
    ?class_name ?signature_db ?builtin_signature_db fdef =
  check_fundef_with_cfg taint_inst shared_tables name ?glob_env ?class_name ?signature_db
    ?builtin_signature_db (CFG_build.cfg_of_gfdef taint_inst.lang fdef)

(* The implicit receiver is reached as [BThis] not [BArg], so stripping it
   keeps [BArg] indices aligned. *)
let is_implicit_receiver (lang : Lang.t) ~(is_first : bool) (info : fun_info)
    (gparam : G.parameter) : bool =
  Receiver.implicit_param lang ~is_method:(Receiver.is_method info.fdef)
    ~is_static:info.is_static ~is_first gparam

let get_arity params info lang =
  Receiver.arity lang ~is_method:(Receiver.is_method info.fdef)
    ~is_static:info.is_static params

(* Drop implicit-receiver IL params; G and IL param lists share length/order. *)
let filter_implicit_receiver_params (lang : Lang.t) (info : fun_info)
    (g_params : G.parameter list) (il_params : IL.param list)
    : IL.param list =
  match info.class_name_str with
  | None -> il_params
  | Some _ ->
    List.combine g_params il_params
    |> List.filteri
         (fun i ((gp, ip) : G.parameter * IL.param) ->
            match ip with
            (* Keep IL.ParamReceiver — extractor maps it to BThis without consuming an arg index. *)
            | IL.ParamReceiver _ -> true
            | _ -> not (is_implicit_receiver lang ~is_first:(i =*= 0) info gp))
    |> List.map snd

(* [fid_filter] skips IL/CFG build for out-of-subgraph fns.  Records get [taint_inst] = [None]; callers set it when needed. *)
let build_info_map
    ~(lang : Lang.t)
    ?(fid_filter : (Function_id.t -> bool) option)
    (ast : G.program)
    : fun_info Shape_and_sig.FunctionMap.t =
  let add_info (fid : Function_id.t) (info : fun_info)
      (info_map : fun_info Shape_and_sig.FunctionMap.t) =
    if Shape_and_sig.FunctionMap.mem fid info_map then info_map
    else Shape_and_sig.FunctionMap.add fid info info_map
  in
  let build_fun_info (name : IL.name) ~(class_name_str : string option)
      ~(method_properties : G.expr list) ~(is_static : bool)
      ~(is_lambda_assignment : bool)
      (fdef : G.function_definition) : fun_info =
    let cfg = CFG_build.cfg_of_gfdef lang fdef in
    { name; class_name_str; method_properties; is_static;
      cfg; fdef; is_lambda_assignment;
      taint_inst = None }
  in
  let info_map =
    Visit_function_defs.fold_with_parent_path ~lang
      (fun info_map opt_ent parent_path fdef ->
        match fst fdef.fkind with
        | LambdaKind
        | Arrow ->
            (* Must match [Graph_from_AST.fn_id_of_entity]'s key, else info_map/topo-fold lookups miss the lambda. *)
            let name = Visit_function_defs.synth_lambda_il_name fdef in
            let fid = Function_id.of_il_name name in
            (match fid_filter with
             | Some f when not (f fid) -> info_map
             | _ ->
               let class_name_str =
                 match parent_path with
                 | Some class_il :: _ -> Some (fst class_il.IL.ident)
                 | _ -> None
               in
               let info =
                 build_fun_info name ~class_name_str
                   ~method_properties:[] ~is_static:false
                   ~is_lambda_assignment:true fdef
               in
               add_info fid info info_map)
        | Function
        | Method
        | BlockCases -> (
            match Option.bind opt_ent AST_to_IL.name_of_entity with
            | None -> info_map
            | Some name ->
                let fid = Function_id.of_il_name name in
                (match fid_filter with
                 | Some f when not (f fid) -> info_map
                 | _ ->
                   let go_receiver_name =
                     match lang with
                     | Lang.Go ->
                         Graph_from_AST.extract_go_receiver_type fdef
                     | _ -> None
                   in
                   let class_name_str =
                     match go_receiver_name with
                     | Some recv_name -> Some recv_name
                     | None -> (
                         match parent_path with
                         | Some class_il :: _ -> Some (fst class_il.IL.ident)
                         | _ -> None)
                   in
                   let has_receiver =
                     let (_, params, _) = fdef.fparams in
                     List.exists
                       (function G.ParamReceiver _ -> true | _ -> false)
                       params
                   in
                   let method_properties =
                     match fst fdef.fkind with
                     | Method ->
                         Taint_signature_extractor.extract_method_properties fdef
                     | Function when has_receiver ->
                         (* Rust: fkind=Function but has ParamReceiver (self) *)
                         Taint_signature_extractor.extract_method_properties fdef
                     | Function | LambdaKind | Arrow | BlockCases -> []
                   in
                   let is_static =
                     match opt_ent with
                     | Some { G.attrs; _ } ->
                         List.exists
                           (function
                             | G.KeywordAttr (G.Static, _) -> true
                             | _ -> false)
                           attrs
                     | None -> false
                   in
                   let info =
                     build_fun_info name ~class_name_str ~method_properties
                       ~is_static ~is_lambda_assignment:false fdef
                   in
                   add_info fid info info_map)))
      Shape_and_sig.FunctionMap.empty
      ast
  in
  info_map

(* Extract a function's taint signature(s) into [db], RETURNING the freshly
   extracted signatures (not just the updated db).  The SCC signature fixpoint
   in [Interfile_dispatch] needs the fresh sigs to REPLACE a function's entry
   each iteration: accumulating them across iterations leaves several
   same-arity sigs in the per-function set, which makes [find_by_arity] give up
   (returning [None]) so callers lose the signature entirely. *)
let extract_signatures
    ?(builtin_signature_db : Shape_and_sig.builtin_signature_database option)
    ~(lang : Lang.t)
    ~(db : Shape_and_sig.signature_database)
    ~(taint_inst : Taint_rule_inst.t)
    ~(shared_tables : Taint_shared_tables.t)
    (info : fun_info)
    : Shape_and_sig.signature_database * Shape_and_sig.extended_sig list =
  let to_ext (sig_, arity) : Shape_and_sig.extended_sig =
    { Shape_and_sig.sig_; arity }
  in
  let params = Tok.unbracket info.fdef.G.fparams in
  let arity = get_arity params info lang in
  let sig_cfg =
    let filtered_params =
      filter_implicit_receiver_params lang info params info.cfg.IL.params
    in
    { info.cfg with IL.params = filtered_params }
  in
  let arity_t = Shape_and_sig.Arity_exact arity in
  let db', sig_ =
    Taint_signature_extractor.extract_signature_with_file_context
      ~arity:arity_t ~db ?builtin_signature_db taint_inst shared_tables
      ~name:info.name
      ~method_properties:info.method_properties
      sig_cfg
  in
  let fresh = [ to_ext (sig_, arity_t) ] in
  (* Kotlin trailing-lambda syntax f(a){b}: also extract at arity-1. *)
  if Lang.equal lang Lang.Kotlin && arity >= 1 then
    match List.rev params with
    | G.Param { G.ptype = Some { t = G.TyFun _; _ }; _ } :: _ ->
        let arity_t' = Shape_and_sig.Arity_exact (arity - 1) in
        let db'', sig_' =
          Taint_signature_extractor.extract_signature_with_file_context
            ~arity:arity_t' ~db:db' ?builtin_signature_db taint_inst shared_tables
            ~name:info.name ~method_properties:info.method_properties
            sig_cfg
        in
        (db'', fresh @ [ to_ext (sig_', arity_t') ])
    | _ -> (db', fresh)
  else (db', fresh)

let extract_and_check
    ?(builtin_signature_db : Shape_and_sig.builtin_signature_database option)
    ?(glob_env : Lval_env.t option)
    ~(lang : Lang.t)
    ~(db : Shape_and_sig.signature_database)
    ~(match_on : [ `Sink | `Source ])
    ~(taint_inst : Taint_rule_inst.t)
    ~(shared_tables : Taint_shared_tables.t)
    ~(detect_findings : bool)
    ~(retention : retention option)
    (info : fun_info)
    : Shape_and_sig.signature_database * PM.t list =
  let updated_db, _fresh_sigs =
    extract_signatures ?builtin_signature_db ~lang ~db
      ~taint_inst ~shared_tables info
  in
  (* For lambda assignments, keep only ToSink effects with a concrete Src match; parameterized (BArg) taint rides the signature instead. *)
  let keep_src_toSink_only (eff : Effect.t) : Effect.t option =
    match eff with
    | Effect.ToSink si ->
        let items, precond = si.taints_with_precondition in
        let src_items =
          List.filter
            (fun (i : Effect.taint_to_sink_item) ->
              match i.taint.orig with
              | Taint.Src _ -> true
              | _ -> false)
            items
        in
        if List_.null src_items then None
        else
          Some
            (Effect.ToSink
               {
                 si with
                 (* [precond] is a formula over labels; evaluated against
                    the surviving Src items' labels it keeps a multi-label
                    [requires] enforced. A requirement satisfied only by a
                    BArg-carried label absent from this slice is checked
                    when the signature is instantiated. *)
                 taints_with_precondition = (src_items, precond);
               })
    | _ -> None
  in
  if (not detect_findings) then
    (updated_db, [])
  else
    let _flow, fdef_effects, _mapping =
      check_fundef_with_cfg taint_inst shared_tables info.name
        ?glob_env ?class_name:info.class_name_str
        ~signature_db:updated_db ?builtin_signature_db
        info.cfg
    in
    let effects_to_record =
    if info.is_lambda_assignment then
      Effects.filter_map ~merge:taint_inst.merge keep_src_toSink_only
        fdef_effects
    else fdef_effects
  in
    let checked_function =
      retention
      |> Option.map (fun (retention : retention) ->
             {
               retention;
               cfg = info.cfg;
               reanalyse =
                 Some
                   (fun (retained : Shape_and_sig.signature_database option) ->
                     let taint_inst = retaining taint_inst in
                     let shared_tables = retention.retain_tables in
                     let db, _fresh =
                       extract_signatures ?builtin_signature_db ~lang
                         ~db:(Option.value retained ~default:db)
                         ~taint_inst ~shared_tables info
                     in
                     let _flow, effects, _mapping =
                       check_fundef_with_cfg taint_inst shared_tables info.name
                         ?glob_env ?class_name:info.class_name_str
                         ~signature_db:db ?builtin_signature_db info.cfg
                     in
                     if info.is_lambda_assignment then
                       Effects.filter_map ~merge:taint_inst.merge
                         keep_src_toSink_only effects
                     else effects);
             })
    in
    let findings = pms_of_effects ~lang ~match_on ~checked_function effects_to_record in
    (updated_db, findings)

(* Class-body initialisers/static blocks aren't call-graph functions, except the class initialiser of a language whose class header is the constructor, which the function passes analyse when [initialisers_are_functions].  CFG build is lang+AST only, so it's split out for multi-rule reuse. *)
let build_class_init_cfgs ~(initialisers_are_functions : bool)
    (lang : Lang.t) (ast : G.program)
    : (IL.name option * IL.fun_cfg) list =
  let acc = ref [] in
  let analysed_as_function (opt_ent : G.entity option)
      (cdef : G.class_definition) : bool =
    initialisers_are_functions
    && Lang_config.class_header_is_constructor lang
    &&
    match opt_ent with
    | Some ent ->
        Option.is_some (Visit_function_defs.initialised_class_name ent cdef)
    | None -> false
  in
  Visit_class_defs.visit
    (fun (opt_ent : G.entity option)
      (cdef : G.class_definition) ->
      if not (analysed_as_function opt_ent cdef) then
      let opt_name =
        let* ent = opt_ent in
        AST_to_IL.name_of_entity ent
      in
      let fields =
        cdef.G.cbody |> Tok.unbracket
        |> List_.map (function G.F x -> x)
        |> G.stmt1
      in
      let stmts = AST_to_IL.stmt lang fields in
      let cfg, lambdas = CFG_build.cfg_of_stmts stmts in
      acc := (opt_name, IL.{ params = []; frettype = None; captures = IL.no_captures; cfg; lambdas }) :: !acc)
    ast;
  !acc

let check_class_inits_prebuilt
    (taint_inst : Taint_rule_inst.t)
    (shared_tables : Taint_shared_tables.t)
    (cfgs : (IL.name option * IL.fun_cfg) list)
    ?(signature_db : Shape_and_sig.signature_database option)
    ?(builtin_signature_db : Shape_and_sig.builtin_signature_database option)
    () : Shape_and_sig.Effects.t =
  List.fold_left
    (fun acc (opt_name, fun_cfg) ->
      let init_effects, _mapping =
        Dataflow_tainting.fixpoint taint_inst shared_tables ?name:opt_name
          ?signature_db ?builtin_signature_db
          fun_cfg
      in
      Shape_and_sig.Effects.union ~merge:taint_inst.merge init_effects acc)
    Shape_and_sig.Effects.empty cfgs

let check_class_inits
    (taint_inst : Taint_rule_inst.t)
    (shared_tables : Taint_shared_tables.t)
    (ast : G.program)
    ?(signature_db : Shape_and_sig.signature_database option)
    ?(builtin_signature_db : Shape_and_sig.builtin_signature_database option)
    () : Shape_and_sig.Effects.t =
  check_class_inits_prebuilt taint_inst shared_tables
    (build_class_init_cfgs
       ~initialisers_are_functions:taint_inst.options.taint_intrafile
       taint_inst.lang ast)
    ?signature_db ?builtin_signature_db ()

(* Check the top-level statements.
 * In scripting languages it is not unusual to write code outside
 * function declarations and we want to check this too. We simply
 * treat the program itself as an anonymous function. *)
let build_top_level_cfg (lang : Lang.t) (ast : G.program)
    : IL.name * IL.fun_cfg =
  let xs = AST_to_IL.stmt lang (G.stmt1 ast) in
  let cfg, lambdas = CFG_build.cfg_of_stmts xs in
  (Graph_from_AST.top_level_name_of_ast ast, IL.{ params = []; frettype = None; captures = IL.no_captures; cfg; lambdas })

let check_top_level_prebuilt
    (taint_inst : Taint_rule_inst.t)
    (shared_tables : Taint_shared_tables.t)
    ((top_level_name, fun_cfg) : IL.name * IL.fun_cfg)
    ?(signature_db : Shape_and_sig.signature_database option)
    ?(builtin_signature_db : Shape_and_sig.builtin_signature_database option)
    () : Shape_and_sig.Effects.t =
  let top_effects, _mapping =
    Dataflow_tainting.fixpoint taint_inst shared_tables ~name:top_level_name
      ?signature_db ?builtin_signature_db
      fun_cfg
  in
  top_effects

let check_rule per_file_formula_cache (rule : R.taint_rule) match_hook
    ~(shared_tables : Taint_shared_tables.t)
    ?(signature_db : Shape_and_sig.signature_database option)
    ?(builtin_signature_db : Shape_and_sig.builtin_signature_database option)
    ?(local_ast_call_graph : Call_graph.G.t option = None)
    (xconf : Match_env.xconfig) (xtarget : Xtarget.t) =
  Log.info (fun m ->
      m
        "Match_tainting_mode:\n\
         ====================\n\
         Running rule %s\n\
         ===================="
        (Rule_ID.to_string (fst rule.R.id)));
  let match_on = match_on_of_xconf xconf in
  let {
    path = { internal_path_to_content = file; _ };
    xlang;
    lazy_ast_and_errors;
    _;
  } : Xtarget.t =
    xtarget
  in
  let lang =
    match xlang with
    | L (lang, _) -> lang
    | LSpacegrep
    | LAliengrep
    | LRegex ->
        failwith "taint-mode and generic/regex matching are incompatible"
  in
  let (ast, skipped_tokens), parse_time =
    Core_profiling.with_time (fun () -> lazy_force lazy_ast_and_errors)
  in
  let errors = Parse_target.errors_from_skipped_tokens skipped_tokens in
  (* the matching time spans the taint spec, the per-function, class
   * initialisation and top-level fixpoints, up to the report *)
  let match_start = Core_profiling.now () in
  let report_of_matches (matches : PM.t list) :
      Core_profiling.rule_profiling RP.match_result =
    RP.mk_match_result matches errors
      {
        Core_profiling.rule_id = fst rule.R.id;
        rule_parse_time = parse_time;
        rule_match_time = Core_profiling.since match_start;
      }
  in
  (* TODO: 'debug_taint' should just be part of 'res'
   * (i.e., add a "debugging" field to 'Report.match_result'). *)
  match
    Match_taint_spec.taint_config_of_rule ~per_file_formula_cache
      xconf lang file (ast, []) rule
  with
  | None -> (Some (report_of_matches []), None)
  | Some (taint_inst, spec_matches, expls) ->
      (* Must match the root used to absolutify the graph/fids below, else dataflow's [Tok.abs_tok] tokens stay relative and miss the graph. *)
      let taint_inst =
        { taint_inst with Taint_rule_inst.project_root = xtarget.project_root }
      in
      let glob_env, glob_effects = Taint_input_env.mk_file_env taint_inst shared_tables ast in
      let glob_matches = pms_of_effects ~lang ~match_on ~checked_function:None glob_effects in

      let final_signature_db, branch_matches, retention =
        if taint_inst.options.taint_intrafile then (
          let call_graph =
            match local_ast_call_graph with
            | Some graph -> graph
            | None ->
                (* No pre-computed graph (e.g. [opengrep show]); build from AST, mirroring check_rules. *)
                Object_initialization.(
                  stamp_id_types (detect_object_initialization ast lang) ast);
                let call_graph =
                  Graph_from_AST.build_call_graph ~lang ast
                in
                (match xtarget.project_root with
                 | Some root -> Call_graph.make_paths_absolute root call_graph
                 | None -> call_graph)
          in
          (* Build user signature database *)
          let initial_signature_db =
            Builtin_models.init_signature_database signature_db
          in

          (* Absolutify keys + info.name to match absolute-path graph vertices. *)
          let info_map =
            let raw = build_info_map ~lang ast in
            match xtarget.project_root with
            | None -> raw
            | Some root ->
              Shape_and_sig.FunctionMap.fold
                (fun (fid : Function_id.t) (info : fun_info) acc ->
                  Shape_and_sig.FunctionMap.add
                    (Function_id.make_absolute root fid)
                    { info with name = IL.absolutify_name xtarget.project_root info.name }
                    acc)
                raw Shape_and_sig.FunctionMap.empty
          in
          let source_ranges =
            (spec_matches.Match_taint_spec.sources
            |> List.map (fun (rwm, _src) -> rwm.Range_with_metavars.r))
            @ Taint_input_env.ranges_of_tainted_globals_in_functions glob_env
                ast
          in
          let sink_ranges =
            spec_matches.Match_taint_spec.sinks
            |> List.map (fun (rwm, _sink) -> rwm.Range_with_metavars.r)
          in
          let absolutify_fid =
            Interfile_graph.absolutify_fid xtarget.project_root
          in
          let source_functions =
            Graph_from_AST.find_functions_containing_ranges ~lang ast
              source_ranges
            |> List.map absolutify_fid
          in
          let sink_functions =
            Graph_from_AST.find_functions_containing_ranges ~lang ast
              sink_ranges
            |> List.map absolutify_fid
          in
          Log.debug (fun m ->
              m "SUBGRAPH: Found %d source functions and %d sink functions"
                (List.length source_functions)
                (List.length sink_functions));
          (* unbounded: no depth, so never cut *)
          let relevant_graph, _cut =
            Graph_reachability.compute_relevant_subgraph call_graph
              ~sources:source_functions ~sinks:sink_functions
          in

          let analysis_order = Call_graph.topological_order relevant_graph in
          let sccs = Sig_fixpoint.sccs_callees_first relevant_graph in
          (* A member of a recursive component composes its offsets under
             the flat bound, as the interfile path does. *)
          let recursive_fids =
            Sig_fixpoint.recursive_members relevant_graph sccs
            |> List.fold_left
                 (fun acc fid -> Shape_and_sig.FunctionMap.add fid () acc)
                 Shape_and_sig.FunctionMap.empty
          in
          let taint_inst_of (node : Function_id.t) : Taint_rule_inst.t =
            { taint_inst with
              Taint_rule_inst.recursive =
                Shape_and_sig.FunctionMap.mem node recursive_fids }
          in
          (* A function's own signatures replace its db entry
             ([Sig_fixpoint.store]). *)
          let analyze (node : Function_id.t)
              (db : Shape_and_sig.signature_database)
              : Shape_and_sig.signature_database =
            match Shape_and_sig.FunctionMap.find_opt node info_map with
            | None -> db
            | Some info ->
              let db', fresh =
                extract_signatures ?builtin_signature_db
                  ~lang ~db
                  ~taint_inst:(taint_inst_of node) ~shared_tables info
              in
              Sig_fixpoint.store ~merge:taint_inst.merge node fresh db'
          in
          let signature_db_after_order =
            Sig_fixpoint.run ~rule_id:(fst rule.R.id) ~graph:relevant_graph
              ~sccs ~analyze initial_signature_db
          in
          let retention =
            mk_retention ~lang
              ~cfg_of:(fun (fid : Function_id.t) ->
                Shape_and_sig.FunctionMap.find_opt fid info_map
                |> Option.map (fun (info : fun_info) -> info.cfg))
              ~shared_tables
              ~retain_signature:(fun (tables : Taint_shared_tables.t)
                                     (fid : Function_id.t)
                                     (db : Shape_and_sig.signature_database) ->
                match Shape_and_sig.FunctionMap.find_opt fid info_map with
                | None -> db
                | Some info ->
                    let taint_inst = retaining (taint_inst_of fid) in
                    let db', fresh =
                      extract_signatures ?builtin_signature_db ~lang ~db
                        ~taint_inst ~shared_tables:tables info
                    in
                    Sig_fixpoint.store ~merge:taint_inst.merge fid fresh db')
              (Some signature_db_after_order)
          in
          (* Single match-emission pass over the converged DB. *)
          let topo_matches =
            List.fold_left
              (fun (ms : Core_match.t list) (node : Function_id.t) ->
                match Shape_and_sig.FunctionMap.find_opt node info_map with
                | None -> ms
                | Some info ->
                  (* The re-extraction against the converged db is NOT
                     redundant: [check_fundef_with_cfg] depends on the
                     immediately preceding extraction against a db that
                     already holds this function's own converged signature.
                     Skipping it collapses some sink ranges onto the taint
                     origin (observed on gitlab); see the matching comment in
                     [Interfile_dispatch.topo_fold]. *)
                  let _db, findings =
                    extract_and_check ?builtin_signature_db
                      ~glob_env ~lang
                      ~db:signature_db_after_order ~match_on
                      ~taint_inst:(taint_inst_of node) ~shared_tables
                      ~detect_findings:true ~retention:(Some retention) info
                  in
                  if not (List_.null findings) then
                    Log.debug (fun m ->
                        m "FINDING: rule=%s target=%s fn=%s"
                          (Rule_ID.to_string (fst rule.R.id))
                          (Fpath.to_string file)
                          (IL.show_name info.name));
                  List.rev_append findings ms)
              [] analysis_order
          in
          (Some signature_db_after_order, topo_matches, retention))
        else (
          (* Cross-function taint analysis disabled: use main branch behavior *)
          let retention =
            mk_retention ~lang
              ~cfg_of:(fun (_ : Function_id.t) -> None)
              ~shared_tables
              ~retain_signature:(fun (_ : Taint_shared_tables.t)
                                     (_ : Function_id.t)
                                     (db : Shape_and_sig.signature_database) ->
                db)
              None
          in
          let fdef_matches = ref [] in
          Visit_function_defs.visit
            (fun opt_ent fdef ->
              match fst fdef.fkind with
              | LambdaKind
              | Arrow ->
                  (* We do not need to analyze lambdas here, they will be analyzed
               together with their enclosing function. This would just duplicate
               work. *)
                  ()
              | Function
              | Method
              | BlockCases ->
                  let opt_name =
                    let* ent = opt_ent in
                    AST_to_IL.name_of_entity ent
                  in
                  match opt_name with
                  | None -> ()
                  | Some name ->
                      Log.info (fun m ->
                          m
                            "Match_tainting_mode:\n\
                             --------------------\n\
                             Checking func def: %s\n\
                             --------------------"
                            (IL.str_of_name name));
                      let flow, fdef_effects, _mapping =
                        check_fundef taint_inst shared_tables name ~glob_env
                          ?builtin_signature_db fdef
                      in
                      let checked_function =
                        Some
                          {
                            retention;
                            cfg = flow;
                            reanalyse =
                              Some
                                (fun (_ : Shape_and_sig.signature_database option) ->
                                  let _flow, effects, _mapping =
                                    check_fundef_with_cfg (retaining taint_inst)
                                      retention.retain_tables name ~glob_env
                                      ?builtin_signature_db flow
                                  in
                                  effects);
                          }
                      in
                      fdef_matches :=
                        List.rev_append
                          (pms_of_effects ~lang ~match_on ~checked_function fdef_effects)
                          !fdef_matches)
            ast;
          (None, !fdef_matches, retention))
      in

      let class_init_effects =
        check_class_inits taint_inst shared_tables ast
          ?signature_db:final_signature_db ?builtin_signature_db
          ()
      in
      let class_init_matches =
        pms_of_effects ~lang ~match_on ~checked_function:None class_init_effects
      in

      let top_matches =
        let top_cfg = build_top_level_cfg taint_inst.lang ast in
        let top_effects =
          check_top_level_prebuilt taint_inst shared_tables top_cfg
            ?signature_db:final_signature_db ?builtin_signature_db
            ()
        in
        let checked_function =
          Some
            {
              retention;
              cfg = snd top_cfg;
              reanalyse =
                Some
                  (fun (retained : Shape_and_sig.signature_database option) ->
                    check_top_level_prebuilt (retaining taint_inst)
                      retention.retain_tables top_cfg ?signature_db:retained
                      ?builtin_signature_db ());
            }
        in
        pms_of_effects ~lang ~match_on ~checked_function top_effects
      in
      let matches =
        List.concat
          [ glob_matches; branch_matches; class_init_matches; top_matches ]
        (* same post-processing as for search-mode in Match_rules.ml *)
        |> PM.uniq
        |> PM.no_submatches (* see "Taint-tracking via ranges" *)
        |> force_traces |> match_hook
      in
      let report = report_of_matches matches in
      let explanations =
        if xconf.matching_explanations then
          [
            {
              ME.op = OutJ.Taint;
              children = expls;
              matches = report.matches;
              pos = snd rule.id;
              extra = None;
            };
          ]
        else []
      in
      let report = { report with explanations } in
      (Some report, final_signature_db)

let check_rules ~match_hook
    ~(per_rule_boilerplate_fn :
       R.rule ->
       (unit -> Core_profiling.rule_profiling Core_result.match_result option) ->
       Core_profiling.rule_profiling Core_result.match_result option)
    (rules : R.taint_rule list) (xconf : Match_env.xconfig)
    (xtarget : Xtarget.t) :
    Core_profiling.rule_profiling Core_result.match_result list =
  (* Check for language support warnings when taint_intrafile is enabled *)
  (match rules with
   | rule :: _ -> (
       (* Check if any rule has taint_intrafile enabled *)
       let has_taint_intrafile =
         match rule.options with
         | Some opts -> opts.taint_intrafile
         | None -> xconf.config.taint_intrafile
       in
       if has_taint_intrafile then
         (* Warn for unsupported languages *)
         let lang = xtarget.xlang |> Xlang.to_lang_exn in
         match lang with
         | Lang.Apex
         | Lang.C
         | Lang.Clojure
         | Lang.Cpp
         | Lang.Crystal
         | Lang.Csharp
         | Lang.Dart
         | Lang.Elixir
         | Lang.Go
         | Lang.Java
         | Lang.Js
         | Lang.Julia
         | Lang.Kotlin
         | Lang.Lua
         | Lang.Python
         | Lang.Ruby
         | Lang.Rust
         | Lang.Scala
         | Lang.Swift
         | Lang.Ts
         | Lang.Vb ->
             (* Known supported languages - no warning *)
             ()
         | other_lang ->
             (* Unknown or unsupported language - warn user *)
             Logs.warn (fun m ->
                 m
                   "Cross-function taint analysis (--taint-intrafile) may not \
                    be fully supported for %s. Results may be limited to \
                    intraprocedural analysis only."
                   (Lang.to_string other_lang)))
   | [] -> ());

  (* We create a "formula cache" here, before dealing with individual rules, to
     permit sharing of matches for sources, sanitizers, propagators, and sinks
     between rules.

     In particular, this expects to see big gains due to shared propagators,
     in Semgrep Pro. There may be some benefit in OSS, but it's low-probability.
  *)
  let per_file_formula_cache =
    Formula_cache.mk_specialized_formula_cache rules
  in

  (* The target's language, when a rule has taint_intrafile enabled *)
  let lang_needing_call_graph =
    if
      rules
      |> List.exists (fun (rule : R.taint_rule) ->
             (Match_env.adjust_xconfig_with_rule_options xconf rule.R.options)
               .config
               .taint_intrafile)
    then Result.to_option (Xlang.to_lang xtarget.xlang)
    else None
  in

  (* Pre-compute call graph and builtin db for the target's language.
     The call graph depends on the AST structure and language, so we compute
     it once and share it across rules that need it. *)
  let ast_call_graph =
    lang_needing_call_graph
    |> Option.map (fun (lang : Lang.t) ->
           let ast, _skipped_tokens = lazy_force xtarget.lazy_ast_and_errors in
           Object_initialization.(
             stamp_id_types (detect_object_initialization ast lang) ast);
           let call_graph =
             Graph_from_AST.build_call_graph ~lang ast
           in
           (* Absolutify to match abs_call_tok tokens in Dataflow_tainting. *)
           match xtarget.project_root with
           | Some root -> Call_graph.make_paths_absolute root call_graph
           | None -> call_graph)
  in

  let guard_atoms = Effect_guard.create_atoms () in

  let builtin_db =
    lang_needing_call_graph
    |> Option.map (Builtin_models.create_all_builtin_models ~atoms:guard_atoms)
  in

  let results =
    rules
    |> List.filter_map (fun rule ->
           let xconf =
             Match_env.adjust_xconfig_with_rule_options xconf rule.R.options
           in
           (* Only pass call graph and builtin db if taint_intrafile is enabled for this rule *)
           let rule_local_ast_call_graph, rule_builtin_signature_db =
             if xconf.config.taint_intrafile then (ast_call_graph, builtin_db)
             else (None, None)
           in
           per_rule_boilerplate_fn
             (rule :> R.rule)
             (fun () ->
               Logs_.with_debug_trace ~__FUNCTION__
                 ~pp_input:(fun _ ->
                   "target: "
                   ^ !!(xtarget.path.internal_path_to_content)
                   ^ "\nruleid: "
                   ^ (rule.id |> fst |> Rule_ID.to_string))
                 (fun () ->
                   let report, _signature_db =
                     check_rule per_file_formula_cache rule match_hook
                       ~shared_tables:(Taint_shared_tables.create guard_atoms)
                       ?builtin_signature_db:rule_builtin_signature_db
                       ~local_ast_call_graph:rule_local_ast_call_graph
                       xconf xtarget
                   in
                   report)))
  in

  results
