(* Yoann Padioleau, Iago Abal
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
open IL
module Log = Log_tainting.Log
module G = AST_generic
module F = IL
module D = Dataflow_core
module Var_env = Dataflow_var_env
module VarMap = Var_env.VarMap
module PM = Core_match
module R = Rule
module LV = IL_helpers
module T = Taint
module Lval_env = Taint_lval_env
module Taints = T.Taint_set
module TM = Taint_spec_match
module TRI = Taint_rule_inst
module S = Shape_and_sig.Shape
module Shape = Taint_shape
module Effect = Shape_and_sig.Effect
module Effects = Shape_and_sig.Effects
module Signature = Shape_and_sig.Signature

(* TODO: Rename things to make clear that there are "sub-matches" and there are
 * "best matches". *)

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Tainting dataflow analysis.
 *
 * - This is a rudimentary taint analysis in some ways, but rather complex in
 *   other ways... We don't do alias analysis, and inter-procedural support
 *   (for DeepSemgrep) still doesn't cover some common cases. On the other hand,
 *   almost _anything_ can be a source/sanitizer/sink, we have taint propagators,
 *   etc.
 * - It is a MAY analysis, it finds *potential* bugs (the tainted path could not
 *   be feasible in practice).
 * - Field sensitivity is limited to l-values of the form x.a.b.c, see module
 *   Taint_lval_env and check_tainted_lval for more details. Very coarse grained
 *   otherwise, e.g. `x[i] = tainted` will taint the whole array,
 *
 * old: This was originally in src/analyze, but it now depends on
 *      Pattern_match, so it was moved to src/engine.
 *)

module DataflowX = Dataflow_core.Make (struct
  type node = F.node
  type edge = F.edge
  type flow = (node, edge) CFG.t

  let short_string_of_node n = Display_IL.short_string_of_node_kind n.F.n
end)

module SMap = Map.Make (String)

let sigs_tag = Log_tainting.sigs_tag
let transfer_tag = Log_tainting.transfer_tag

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type mapping = Lval_env.t D.mapping
type java_props_cache = (string * G.SId.t, IL.name) Hashtbl.t

let mk_empty_java_props_cache () = Hashtbl.create 30

type func = {
  name : IL.name option;
  sig_params : Signature_params.params;
      (** Signature-level parameters of the function under analysis. Used to
          synthesise a "self-sig" view of the in-progress signature for
          self-recursive calls (see [lookup_signature_with_object_context]). *)
  il_params : IL.param list;
      (** The same parameters in their [IL.param] form. Used by
          [Sig_inst.instantiate_function_signature] to rebind a callee's
          guards into this function's parameters when the called sig
          carries an unresolved guard. *)
  param_sids : G.SId.t list;
      (** [sid]s of [il_params], precomputed for the call-effect
          [ToLval] consumer's bound-vs-free check. *)
  captured : (IL.name * AST_generic.capture_mode) list Lazy.t;
      (** Variables of enclosing functions that the function reads or
          writes, see [Signature.captured]. *)
  best_matches : TM.Best_matches.t;
      (** Best matches for the taint sources/etc, see 'Taint_spec_match'. *)
  used_lambdas : IL.NameSet.t;
      (** Set of lambda names that are *used* within the function. If a lambda
          is used, we analyze it at use-site, otherwise we analyze it at def
          site. *)
}
(** Data about the top-level function definition under analysis, this does not *
    vary when analyzing lambdas. *)

(* REFACTOR: Rename 'Taint_lval_env' as 'Taint_var_env' and create a new module
    for this 'env' type called 'Taint_env' or 'Taint_state' or sth, then we could
    e.g. move all lambda stuff to 'Taint_lambda'. *)
(* THINK: Separate read-only enviroment into a new a "cfg" type? *)
type env = {
  taint_inst : Taint_rule_inst.t;
  shared_tables : Taint_shared_tables.t;
  func : func;
  in_lambda : IL.name option;
  needed_vars : IL.NameSet.t;
      (** Vars that we need to track in the current function/lambda under
          analysis, other vars can be filtered out, see 'fixpoint_lambda' as
          well as 'Taint_lambda.find_vars_to_track_across_lambdas'. *)
  lval_env : Lval_env.t;
  effects_acc : Effects.t ref;
  did_self_recurse : bool ref;
      (** Set to [true] when [self_sig_if_recursive] returns a sig during the
          current pass. Used to gate the outer self-sig convergence loop in
          [fixpoint_aux]: a pass without any self-recursive call needs no
          retry. *)
  signature_db : Shape_and_sig.signature_database option;
      (** Signature database for inter-procedural taint analysis *)
  builtin_signature_db : Shape_and_sig.builtin_signature_database option;
      (** Builtin signature database for standard library functions *)
}

(*****************************************************************************)
(* Options *)
(*****************************************************************************)

let propagate_through_functions env =
  (not env.taint_inst.options.taint_assume_safe_functions)
  && not env.taint_inst.options.taint_only_propagate_through_assignments

let propagate_through_indexes env =
  (not env.taint_inst.options.taint_assume_safe_indexes)
  && not env.taint_inst.options.taint_only_propagate_through_assignments

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
let add_taints_from_shape shape =
  Taints.union (Shape.gather_all_taints_in_shape shape)

let log_timeout_warning (taint_inst : Taint_rule_inst.t) opt_name timeout =
  match timeout with
  | `Ok -> ()
  | `Capped ->
      Log.debug (fun m ->
          m "Fixpoint visit cap reached [rule: %s file: %s func: %s]"
            (Rule_ID.to_string taint_inst.rule_id)
            !!(taint_inst.file)
            (Option.map IL.str_of_name opt_name ||| "???"))
  | `Timeout ->
      (* nosemgrep: no-logs-in-library *)
      Logs.warn (fun m ->
          m
            "Fixpoint timeout while performing taint analysis [rule: %s file: \
             %s func: %s]"
            (Rule_ID.to_string taint_inst.rule_id)
            !!(taint_inst.file)
            (Option.map IL.str_of_name opt_name ||| "???"))

let map_check_expr env check_expr xs =
  let rev_taints_and_shapes, lval_env =
    xs
    |> List.fold_left
         (fun (rev_taints_and_shapes, lval_env) x ->
           let taints, shape, lval_env = check_expr { env with lval_env } x in
           ((taints, shape) :: rev_taints_and_shapes, lval_env))
         ([], env.lval_env)
  in
  (List.rev rev_taints_and_shapes, lval_env)

let union_map_taints_and_vars env check xs =
  let taints, lval_env =
    xs
    |> List.fold_left
         (fun (taints_acc, lval_env) x ->
           let taints, shape, lval_env = check { env with lval_env } x in
           let taints_acc =
             taints_acc |> Taints.union taints |> add_taints_from_shape shape
           in
           (taints_acc, lval_env))
         (Taints.empty, env.lval_env)
  in
  let taints =
    if env.taint_inst.options.taint_only_propagate_through_assignments then
      Taints.empty
    else taints
  in
  (taints, lval_env)

let gather_all_taints_in_args_taints args_taints =
  args_taints
  |> List.fold_left
       (fun acc arg ->
         match arg with
         | Named (_, (_, shape))
         | Unnamed (_, shape) ->
             Shape.gather_all_taints_in_shape shape |> Taints.union acc)
       Taints.empty

let any_is_best_sanitizer env any =
  env.taint_inst.preds.is_sanitizer any
  |> List.filter (fun (m : R.taint_sanitizer TM.t) ->
         (not m.spec.sanitizer_exact)
         || TM.is_best_match env.func.best_matches m)

(* TODO: We could return source matches already split by `by-side-effect` here ? *)
let any_is_best_source ?(is_lval = false) env any =
  env.taint_inst.preds.is_source any
  |> List.filter (fun (m : R.taint_source TM.t) ->
         match m.spec.source_by_side_effect with
         | Only -> is_lval && TM.is_exact m
         (* 'Yes' should probably require an exact match like 'Only' but for
          *  backwards compatibility we keep it this way. *)
         | Yes
         | No ->
             (not m.spec.source_exact)
             || TM.is_best_match env.func.best_matches m)

let any_is_best_sink env any =
  env.taint_inst.preds.is_sink any
  |> List.filter (fun (tm : R.taint_sink TM.t) ->
         (* at-exit sinks are filtered out and never reported *)
         (not tm.spec.sink_at_exit) && TM.is_best_match env.func.best_matches tm)

let orig_is_source (taint_inst : Taint_rule_inst.t) orig =
  taint_inst.preds.is_source (any_of_orig orig)

let orig_is_best_source env orig : R.taint_source TM.t list =
  any_is_best_source env (any_of_orig orig)
[@@profiling]

let orig_is_sanitizer (taint_inst : Taint_rule_inst.t) orig =
  taint_inst.preds.is_sanitizer (any_of_orig orig)

let orig_is_best_sanitizer env orig =
  any_is_best_sanitizer env (any_of_orig orig)
[@@profiling]

let orig_is_sink (taint_inst : Taint_rule_inst.t) orig =
  taint_inst.preds.is_sink (any_of_orig orig)

let orig_is_best_sink env orig = any_is_best_sink env (any_of_orig orig)
[@@profiling]

let any_of_lval lval =
  match lval with
  | { rev_offset = { oorig; _ } :: _; _ } -> any_of_orig oorig
  | { base = Var var; rev_offset = [] } ->
      let _, tok = var.ident in
      G.Tk tok
  | { base = VarSpecial (_, tok); rev_offset = [] } -> G.Tk tok
  | { base = Mem e; rev_offset = [] } -> any_of_orig e.eorig

let lval_is_source env lval =
  any_is_best_source ~is_lval:true env (any_of_lval lval)

let lval_is_best_sanitizer env lval =
  any_is_best_sanitizer env (any_of_lval lval)

let lval_is_sink env lval =
  let any = any_of_lval lval in
  let sinks = env.taint_inst.preds.is_sink any in
  sinks
  |> List.filter (fun (tm : R.taint_sink TM.t) ->
         (* at-exit sinks are filtered out and never reported *)
         not tm.spec.sink_at_exit)
[@@profiling]

let taints_of_matches env ~incoming sources =
  let control_sources, data_sources =
    sources
    |> List.partition (fun (m : R.taint_source TM.t) -> m.spec.source_control)
  in
  (* THINK: It could make sense to merge `incoming` with `control_incoming`, so
   * a control source could influence a data source and vice-versa. *)
  let data_taints =
    data_sources
    |> List_.map (fun x -> (x.TM.spec_pm, x.spec))
    |> T.taints_of_pms ~incoming
  in
  let control_incoming = Lval_env.get_control_taints env.lval_env in
  let control_taints =
    control_sources
    |> List_.map (fun x -> (x.TM.spec_pm, x.spec))
    |> T.taints_of_pms ~incoming:control_incoming
  in
  let lval_env = Lval_env.add_control_taints env.lval_env control_taints in
  (data_taints, lval_env)

let record_effects env new_effects =
  Log.debug (fun m ->
      let fn = Option.fold ~none:"<anon>" ~some:IL.str_of_name env.func.name in
      let effects_str =
        new_effects |> List.map Effect.show |> String.concat " ; "
      in
      m "REC_EFFECTS in %s: [%s]" fn effects_str);
  if Lval_env.is_dead env.lval_env then
    (* Unreachable program point — no findings, no signature effects. *)
    ()
  else if not (List_.null new_effects) then
    let new_effects =
      env.taint_inst.handle_effects env.func.name new_effects
    in
    (* Stamp each new effect with the guards active at the current program
     * point, so a caller can drop effects whose guard its argument shape
     * cannot satisfy. When no guard is active, [add_guards] is a no-op.
     * [live_guards] drops any guard whose variable has since been
     * reassigned, since the non-SSA IL would otherwise evaluate it against
     * a stale value at the caller. *)
    let active = Lval_env.live_guards env.lval_env in
    let new_effects =
      if Effect_guard.Set.is_empty active then new_effects
      else (
        Log.debug (fun m ->
            m "GUARD_STAMP: stamping %d effect(s) with %s"
              (List.length new_effects)
              (Effect_guard.show_set active));
        let g = Effect_guard.conjoin (Effect_guard.Set.elements active) in
        List.map (Effect.add_guards g) new_effects)
    in
    (* Widen the recorded shapes ([ToReturn] data shapes, [ToSinkInCall]
     * argument shapes). A self-recursive tree-builder — one that wraps its
     * own recursive result in a fresh container — has no fixpoint in the
     * shape domain: the recursive call sees the in-progress effects via
     * [self_sig_if_recursive], so each pass of the INNER dataflow fixpoint
     * nests the return shape one level deeper (and branch unification can
     * double the node count per pass), running the clock out on the taint
     * fixpoint timeout with a huge lval_env. Truncating every effect as it
     * is recorded cuts that ascending chain where it feeds back, and bounds
     * the shapes stored in signature databases (SCC-level recursion
     * included). The cut is the longest offset a lookup can form: no level
     * below it is ever read, and a builder of [k] fields keeps a [k]-way
     * tree of the cut depth. *)
    let new_effects =
      new_effects
      |> List_.map
           (Shape.truncate_effect
              ~max_depth:(Shape.max_poly_offset env.taint_inst.lang))
    in
    env.effects_acc := Effects.add_list new_effects !(env.effects_acc)

(* Field write on the enclosing receiver: record [BThis] so it composes
   into this function's signature. *)
let record_this_field_write env taints offset guards =
  record_effects env
    [ Effect.ToLval
        { taints; lval = { base = Taint.BThis; offset }; guards } ]

(* Also reflect a this-field write in the local [lval_env], mirroring the
   sibling [ToLval] arm, so a later read of the same field in THIS function
   sees it. [this.x…] normalizes to the field [x] as a var (see
   [normalize_lval]); not representable when the offset does not start with a
   field (an index/slice base), in which case the env is unchanged. *)
let add_this_field_to_lval_env env lval_env offset taints =
  match offset with
  | T.Ofld field :: rest ->
      Lval_env.add env.taint_inst.lang field rest taints lval_env
  | _ -> lval_env

(* Own formal parameters are bound in the sig being computed; anything
   else is free. [effects_from_arg_updates_at_exit] handles own params
   on the [BArg] side. *)
let is_own_param (env : env) (var : IL.name) : bool =
  List.exists (G.SId.equal var.IL.sid) env.func.param_sids

(* The base of a write to a variable that is not a parameter of the
 * function under analysis: a variable the function captures is written
 * through its closure's environment. *)
let base_of_free_var (env : env) (var : IL.name) : T.base =
  if
    List.exists
      (fun (x, _) -> Int.equal (IL.compare_name x var) 0)
      (Lazy.force env.func.captured)
  then T.BEnv var
  else T.BGlob var

let mk_param_sids (params : IL.param list) : G.SId.t list =
  List.filter_map
    (fun p ->
      Option.map (fun (n : IL.name) -> n.IL.sid) (IL_helpers.pname_of_param p))
    params

let unify_mvars_sets env mvars1 mvars2 =
  let xs =
    List.fold_left
      (fun xs_opt (mvar, mval) ->
        let* xs = xs_opt in
        match List.assoc_opt mvar mvars2 with
        | None -> Some ((mvar, mval) :: xs)
        | Some mval' when Matching_generic.equal_ast_bound_code
                            env.taint_inst.options mval mval' ->
            Some ((mvar, mval) :: xs)
        | _ -> None)
      (Some []) mvars1
  in
  let ys =
    List.filter (fun (mvar, _) -> not @@ List.mem_assoc mvar mvars1) mvars2
  in
  Option.map (fun xs -> xs @ ys) xs

let sink_biased_union_mvars source_mvars sink_mvars =
  let source_mvars' =
    List.filter
      (fun (mvar, _) -> not @@ List.mem_assoc mvar sink_mvars)
      source_mvars
  in
  Some (source_mvars' @ sink_mvars)

(* Takes the bindings of multiple taint sources and filters the bindings ($MVAR, MVAL)
 * such that either $MVAR is bound by a single source, or all MVALs bounds to $MVAR
 * can be unified. *)
let merge_source_mvars env bindings =
  let flat_bindings = List_.flatten bindings in
  let bindings_tbl =
    flat_bindings
    |> List_.map (fun (mvar, _) -> (mvar, None))
    |> List.to_seq |> Hashtbl.of_seq
  in
  flat_bindings
  |> List.iter (fun (mvar, mval) ->
         match Hashtbl.find_opt bindings_tbl mvar with
         | None ->
             (* This should only happen if we've previously found that
                there is a conflict between bound values at `mvar` in
                the sources.
             *)
             ()
         | Some None ->
             (* This is our first time seeing this value, let's just
                add it in.
             *)
             Hashtbl.replace bindings_tbl mvar (Some mval)
         | Some (Some mval') ->
             if
               not
                 (Matching_generic.equal_ast_bound_code env.taint_inst.options
                    mval mval')
             then Hashtbl.remove bindings_tbl mvar);
  (* After this, the only surviving bindings should be those where
     there was no conflict between bindings in different sources.
  *)
  bindings_tbl |> Hashtbl.to_seq |> List.of_seq
  |> List.sort (fun (mvar1, _) (mvar2, _) -> String.compare mvar1 mvar2)
  |> List_.filter_map (fun (mvar, mval_opt) ->
         match mval_opt with
         | None ->
             (* This actually shouldn't really be possible, every
                binding should either not exist, or contain a value
                if there's no conflict. But whatever. *)
             None
         | Some mval -> Some (mvar, mval))

(* Merge source's and sink's bound metavariables. *)
let merge_source_sink_mvars env source_mvars sink_mvars =
  if env.taint_inst.options.taint_unify_mvars then
    (* This used to be the default, but it turned out to be confusing even for
     * r2c's security team! Typically you think of `pattern-sources` and
     * `pattern-sinks` as independent. We keep this option mainly for
     * backwards compatibility, it may be removed later on if no real use
     * is found. *)
    unify_mvars_sets env source_mvars sink_mvars
  else
    (* The union of both sets, but taking the sink mvars in case of collision. *)
    sink_biased_union_mvars source_mvars sink_mvars

let partition_sources_by_side_effect sources_matches =
  sources_matches
  |> Either_.partition_either3 (fun (m : R.taint_source TM.t) ->
         match m.spec.source_by_side_effect with
         | R.Only -> Left3 m
         (* A 'Yes' should be a 'Yes' regardless of whether the match is exact...
          * Whether the match is exact or not is/should be taken into consideration
          * later on. Same as for 'Only'. But for backwards-compatibility we keep
          * it this way for now. *)
         | R.Yes when TM.is_exact m -> Middle3 m
         | R.Yes
         | R.No ->
             Right3 m)
  |> fun (only, yes, no) -> (`Only only, `Yes yes, `No no)

(* We need to filter out `Control` variables since those do not propagate trough return
 * (there is just no point in doing so). *)
let get_control_taints_to_return env =
  Lval_env.get_control_taints env.lval_env
  |> Taints.filter (fun (b : T.guarded_taint) ->
         match b.taint.orig with
         | T.Src _ -> true
         | Var _
         | Shape_var _
         | Control ->
             false)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

let type_of_lval env lval =
  match lval with
  | { base = Var x; rev_offset = [] } ->
      Typing.resolved_type_of_id_info env.taint_inst.lang x.id_info
  | { base = _; rev_offset = { o = Dot fld; _ } :: _ } ->
      Typing.resolved_type_of_id_info env.taint_inst.lang fld.id_info
  | __else__ -> Type.NoType

let type_of_expr env e =
  match e.eorig with
  | SameAs eorig -> Typing.type_of_expr env.taint_inst.lang eorig |> fst
  | __else__ -> Type.NoType

(* We only check this at a few key places to avoid calling `type_of_expr` too
 * many times which could be bad for perf (but haven't properly benchmarked):
 * - assignments
 * - return's
 * - function calls and their actual arguments
 * TODO: Ideally we add an `e_type` field and have a type-inference pass to
 *  fill it in, so that every expression has its known type available without
 *  extra cost.
 *)
let drop_taints_if_bool_or_number (options : Rule_options.t) taints ty =
  match ty with
  | Type.(Builtin Bool) when options.taint_assume_safe_booleans -> Taints.empty
  | Type.(Builtin (Int | Float | Number)) when options.taint_assume_safe_numbers
    ->
      Taints.empty
  | __else__ -> taints

(* Calls to 'type_of_expr' seem not to be cheap and even though we tried to limit the
 * number of these calls being made, doing them unconditionally caused a slowdown of
 * ~25% in a ~dozen repos in our stress-test-monorepo. We should just not call
 * 'type_of_expr' unless at least one of the taint_assume_safe_{booleans,numbers} has
 * been set, so rules that do not use these options remain unaffected. Long term we
 * should make type_of_expr less costly.
 *)
let check_type_and_drop_taints_if_bool_or_number env taints type_of_x x =
  if
    (env.taint_inst.options.taint_assume_safe_booleans
   || env.taint_inst.options.taint_assume_safe_numbers)
    && not (Taints.is_empty taints)
  then
    match type_of_x env x with
    | Type.Function (_, return_ty) ->
        drop_taints_if_bool_or_number env.taint_inst.options taints return_ty
    | ty -> drop_taints_if_bool_or_number env.taint_inst.options taints ty
  else taints

(*****************************************************************************)
(* Labels *)
(*****************************************************************************)

(* This function is used to convert some taint thing we're holding
   to one which has been propagated to a new label.
   See [handle_taint_propagators] for more.
*)
let propagate_taint_to_label replace_labels label (taint : T.taint) =
  let new_orig =
    match (taint.orig, replace_labels) with
    (* if there are no replaced labels specified, we will replace
       indiscriminately
    *)
    | Src src, None -> T.Src { src with label }
    | Src src, Some replace_labels
      when List.exists (String.equal src.T.label) replace_labels ->
        T.Src { src with label }
    | ((Src _ | Var _ | Shape_var _ | Control) as orig), _ -> orig
  in
  { taint with orig = new_orig }

(*****************************************************************************)
(* Effects and signatures *)
(*****************************************************************************)

(* Potentially produces an effect from incoming taints + call traces to a sink.
   Note that, while this sink has a `requires` and incoming labels,
   we decline to solve this now!
   We will figure out how many actual Semgrep findings are generated
   when this information is used, later.
*)
(* The items of a sink effect are bounded like a taint set: a call trace
   distinguishes two taints of the same source, and recursion makes them
   without bound. The bound counts the items of each label separately,
   because a sink whose `requires` mentions several labels must still see
   an item of every one of them. *)
let bound_sink_items (taints_with_traces : Effect.taint_to_sink_item list) :
    Effect.taint_to_sink_item list =
  let max = !Flag_semgrep.max_taint_set_size in
  if max =|= 0 || List.compare_length_with taints_with_traces max <= 0 then
    taints_with_traces
  else
    (* Taint that does not come from a source carries no label; those items
       are counted together, under the same bound. *)
    let label_of_item ({ Effect.taint; _ } : Effect.taint_to_sink_item) :
        string option =
      match taint.T.orig with
      | T.Src src -> Some src.T.label
      | Var _
      | Shape_var _
      | Control ->
          None
    in
    let same_label (label1 : string option) (label2 : string option) : bool =
      Option.equal String.equal label1 label2
    in
    (* One pass over the items, keeping the first 'max' of each label in
       their original order; 'taken' holds one entry per distinct label. *)
    let kept, dropped, _taken =
      taints_with_traces
      |> List.fold_left
           (fun (kept, dropped, taken) (item : Effect.taint_to_sink_item) ->
             let label = label_of_item item in
             let n =
               match List.find_opt (fun (l, _) -> same_label l label) taken with
               | Some (_, n) -> n
               | None -> 0
             in
             if n < max then
               ( item :: kept,
                 dropped,
                 (label, n + 1)
                 :: List.filter (fun (l, _) -> not (same_label l label)) taken )
             else (kept, dropped + 1, taken))
           ([], 0, [])
    in
    if dropped =|= 0 then taints_with_traces
    else (
      Log.warn (fun m ->
          m "SINK_ITEMS_SATURATED: cardinal=%d dropping=%d beyond %d per label"
            (List.length taints_with_traces)
            dropped max);
      List.rev kept)

let effects_of_tainted_sink env taints_with_traces (sink : Effect.sink) :
    Effect.t list =
  match bound_sink_items taints_with_traces with
  | [] -> []
  | _ :: _ as taints_with_traces -> (
      (* We cannot check whether we satisfy the `requires` here.
         This is because this sink may be inside of a function, meaning that
         argument taint can reach it, which can only be instantiated at the
         point where we call the function.
         So we record the `requires` within the taint finding, and evaluate
         the formula later, when we extract the PMs
      *)
      let { Effect.pm = sink_pm; rule_sink = ts } = sink in
      let taints_and_bindings =
        taints_with_traces
        |> List_.map (fun ({ Effect.taint; _ } as item) ->
               let bindings =
                 match taint.T.orig with
                 | T.Src source ->
                     let src_pm, _ = T.pm_of_trace source.call_trace in
                     src_pm.env
                 | Var _
                 | Shape_var _
                 | Control ->
                     []
               in
               let new_taint = T.reverse_trace taint in
               ({ item with taint = new_taint }, bindings))
      in
      (* If `unify_mvars` is set, then we will just do the previous behavior,
         and emit a finding for every single source coming into the sink.
         This will mean we don't regress on `taint_unify_mvars: true` rules.

         This is problematic because there may be many sources, all of which do not
         unify with each other, but which unify with the sink.
         If we did as below and unified them all with each other, we would sometimes
         produce no findings when we should.
      *)
      (* The same will happen if our sink does not have an explicit `requires`.

         This is because our behavior in the second case will remove metavariables
         from the finding, if they conflict in the sources.

         This can lead to a loss of metavariable interpolation in the finding message,
         even for "vanilla" taint mode rules that don't use labels, for instance if
         we had two instances of the source

         foo($X)

         reaching a sink, where in both instances, `$X` is not the same. The current
         behavior is that one of the `$X` bindings is chosen arbitrarily. We will
         try to keep this behavior here.
      *)
      if
        env.taint_inst.options.taint_unify_mvars
        || Option.is_none sink.rule_sink.sink_requires
      then
        taints_and_bindings
        |> List_.filter_map (fun (t, bindings) ->
               let* merged_env =
                 merge_source_sink_mvars env sink_pm.PM.env bindings
               in
               Some
                 (Effect.ToSink
                    {
                      taints_with_precondition = ([ t ], R.get_sink_requires ts);
                      sink;
                      merged_env;
                      guards = Effect_guard.top;
                    }))
      else
        match
          taints_and_bindings |> List_.map snd |> merge_source_mvars env
          |> merge_source_sink_mvars env sink_pm.PM.env
        with
        | None -> []
        | Some merged_env ->
            [
              Effect.ToSink
                {
                  taints_with_precondition =
                    (List_.map fst taints_and_bindings, R.get_sink_requires ts);
                  sink;
                  merged_env;
                  guards = Effect_guard.top;
                };
            ])

(* Produces a finding for every unifiable source-sink pair. *)
let effects_of_tainted_sinks env taints sinks : Effect.t list =
  let taints =
    let control_taints = Lval_env.get_control_taints env.lval_env in
    taints |> Taints.union control_taints
  in
  if Taints.is_empty taints then []
  else
    sinks
    |> List.concat_map (fun sink ->
           (* This is where all taint effects start. If it's interproc,
              the call trace will be later augmented into the Call variant,
              but it starts out here as just a PM variant.
           *)
           let taints_with_traces =
             taints |> Taints.elements
             |> List_.map (fun (b : T.guarded_taint) ->
                    { Effect.taint = b.taint;
                      sink_trace = T.PM (sink.Effect.pm, ());
                      guard = b.guard })
           in
           effects_of_tainted_sink env taints_with_traces sink)

(* Go and Lua have no tuple values: a function returning several results
 * returns them as a tuple. In Go the function declares them, so every
 * [return] of it gives several results, [return f()] included; in Lua,
 * which declares no result types, a returned tuple is several results. *)
let returns_several_results (lang : Lang.t) (fun_cfg : IL.fun_cfg)
    (e : IL.exp) : bool =
  match lang with
  | Lang.Go -> (
      match fun_cfg.frettype with
      | Some { t = G.TyTuple _; _ } -> true
      | _ -> false)
  | Lang.Lua -> (
      match e.e with
      | Composite (CTuple, _) -> true
      | _ -> false)
  | _ -> false

let effects_of_tainted_return env ~(several_results : bool) taints shape
    return_tok : Effect.t list =
  let control_taints = get_control_taints_to_return env in
  let relevant_data = Shape.taints_and_shape_are_relevant taints shape in
  let has_ctrl = not (Taints.is_empty control_taints) in
  if (not relevant_data) && not has_ctrl then []
  else
    (* Emit one [ToReturn] per guarded taint [(taint, guard)] so that disjunctive
     * provenance survives instantiation: each guarded taint's [guard] becomes the
     * effect's own [guards] field, which [Sig_inst.classify_guards]
     * evaluates per effect at the caller. *)
    let data_returns =
      if Taints.is_empty taints && relevant_data then
        (* Shape-only return: the value being returned carries no taints
           but its shape is relevant (e.g. a [Fun] shape for a returned
           function). Emit one [ToReturn] carrying the shape so callers
           can attach it to the receiving lval. *)
        [
          Effect.ToReturn
            {
              data_taints = Taints.empty;
              data_shape = shape;
              several_results;
              control_taints = Taints.empty;
              return_tok;
              guards = Effect_guard.top;
            };
        ]
      else
        Taints.elements taints
        |> List.stable_sort (fun (b1 : T.guarded_taint) (b2 : T.guarded_taint) ->
               Effect_guard.compare b1.guard b2.guard)
        |> List.fold_left
             (fun groups (b : T.guarded_taint) ->
               let t = T.lift_taint (T.reverse_trace b.taint) in
               match groups with
               | (guard, group) :: rest when Effect_guard.equal guard b.guard ->
                   (guard, Taints.add t group) :: rest
               | _ -> (b.guard, Taints.add t Taints.empty) :: groups)
             []
        |> List_.map (fun (guard, group) ->
               Effect.ToReturn
                 {
                   data_taints = group;
                   data_shape = shape;
                   several_results;
                   control_taints = Taints.empty;
                   return_tok;
                   guards = guard;
                 })
    in
    let ctrl_return =
      if has_ctrl then
        [
          Effect.ToReturn
            {
              data_taints = Taints.empty;
              data_shape = Bot;
              several_results = false;
              control_taints;
              return_tok;
              guards = Effect_guard.top;
            };
        ]
      else []
    in
    data_returns @ ctrl_return

(* If a 'fun_exp' has no known taint signature, then it should have a polymorphic
 * shape and we record its effects with an "effect variable" (that's kind of what
 * 'ToSinkInCall' does). *)
let effects_of_call_func_arg fun_exp fun_shape args_taints =
  Log.debug (fun m ->
      m "HOF_DISPATCH: callee=%s shape=%s"
        (Display_IL.string_of_exp fun_exp)
        (S.show_shape fun_shape));
  match fun_shape with
  | S.Arg (fun_arg, arg_offsets) ->
      (* One ToSinkInCall per alternative path so the resolver
       * enumerates each possible callback at the call site. *)
      arg_offsets
      |> List.map (fun arg_offset ->
             Effect.ToSinkInCall
               {
                 callee = fun_exp;
                 arg = fun_arg;
                 arg_offset;
                 args_taints;
                 guards = Effect_guard.top;
               })
  | __else__ ->
      Log.debug (fun m ->
          m "Function (?) %s has shape %s"
            (Display_IL.string_of_exp fun_exp)
            (S.show_shape fun_shape));
      []

(* The result of the calls [effects_of_call_func_arg] records. *)
let result_of_call_func_arg ~(lang : Lang.t) fun_exp fun_shape :
    Taints.t * S.shape =
  match fun_shape with
  | S.Arg (fun_arg, arg_offsets) ->
      let loc = T.call_loc_of_exp fun_exp in
      arg_offsets
      |> List.fold_left
           (fun (taints, shape) arg_offset ->
             let call =
               { T.callee = fun_arg; callee_offset = arg_offset; loc }
             in
             ( Taints.union
                 (Taints.singleton
                    (T.taint_of_orig
                       (T.Var { base = T.BCall call; offset = [] })))
                 taints,
               Shape.unify_shape ~lang (S.Arg (T.Result call, [ [] ])) shape ))
           (Taints.empty, S.Bot)
  | __else__ -> (Taints.empty, S.Bot)

(* The signatures of every definition the [id_callee_definition] stamp
   holds; each sid is the definition's site, which keys the signature DB.
   The stamp is trusted whatever name it resolves to, gated only by the
   lookup itself: a bare-name mismatch is as likely to be a deliberate
   alias (a class-body field alias exposes name X for a target named Y,
   and projidx's write-back stamps the target's sid) or a constructor
   (Ruby [Cls.new]->[initialize], Python [Cls()]->[__init__]) as a stale
   stamp, and a stamp that resolves to a stored signature of the right
   arity is evidence enough. *)
let signature_via_callee_definition ~project_root db (id_info : G.id_info)
    arity =
  !(id_info.G.id_callee_definition)
  |> List.filter_map (fun (sid : G.SId.t) ->
         if G.SId.is_unsafe_default sid then None
         else
           (* A project scan keys the sig DB by absolutified fids, while sids
              carry the as-parsed (possibly relative) file. *)
           let fid =
             let fid = Function_id.of_sid sid in
             match project_root with
             | Some root -> Function_id.make_absolute root fid
             | None -> fid
           in
           Shape_and_sig.lookup_definition db fid arity)

(* Helper to fallback to builtin signature database if regular lookup fails *)
let try_builtin_fallback env func_name arity result =
  match result with
  | Some _ -> result
  | None ->
      (match env.builtin_signature_db with
      | Some builtin_db ->
          let builtin_result = Shape_and_sig.(lookup_builtin_signature builtin_db func_name arity) in
          Log.debug (fun m ->
              m "TAINT_SIG: Builtin lookup for %s: %s"
                func_name
                (if Option.is_some builtin_result then "FOUND" else "NOT FOUND"));
          builtin_result
      | None -> None)

(* A built-in model stands for a function the file does not define: the name
   is bound to an import or naming left it unresolved. A [_tmp] the lowering
   creates for a call result carries no binding and may take one. The member
   that Ruby's [method(:f)] denotes has no binding either and takes none: the
   graph resolves it on the receiver's class. *)
let may_take_builtin_model (id_info : G.id_info) : bool =
  match !(id_info.G.id_resolved) with
  | None
  | Some ((G.ImportedEntity _ | G.ImportedModule _), _) ->
      true
  | Some _ -> false

(* The signatures of the definitions a bare name's stamp holds; with no
 * stamp, the built-in model of the name when [may_take_builtin_model]
 * allows one. *)
let lookup_bare_function_name env db (name : IL.name) arity =
  match
    signature_via_callee_definition
      ~project_root:env.taint_inst.project_root db name.IL.id_info arity
  with
  | _ :: _ as found -> found
  | [] when may_take_builtin_model name.IL.id_info ->
      Option.to_list (try_builtin_fallback env (fst name.ident) arity None)
  | [] -> []

let lookup_signature_with_object_context env fun_exp arity =
  Log.debug (fun m ->
      m "TAINT_SIG_LOOKUP: Looking up %s with arity %d"
        (Display_IL.string_of_exp fun_exp) arity);
  match env.signature_db with
  | None ->
      Log.debug (fun m -> m "TAINT_SIG: No signature database available");
      []
  | Some db -> (
      match fun_exp.e with
      | Fetch { base = Var name; rev_offset = [] } ->
          lookup_bare_function_name env db name arity
      | Fetch { base = Var obj; rev_offset = [ { o = Dot method_name; _ } ] } -> (
          match
            signature_via_callee_definition
              ~project_root:env.taint_inst.project_root db
              method_name.id_info arity
          with
          | _ :: _ as found -> found
          | [] ->
              Option.to_list @@
              (* With no stamp: the built-in model of [Mod.f] when the
                 receiver may take one (an Elixir module such as [Enum]), else
                 the built-in model of the method, which belongs to the
                 library type whatever variable holds the value. *)
              let result =
                if may_take_builtin_model obj.id_info then
                  try_builtin_fallback env
                    (fst obj.ident ^ "." ^ fst method_name.ident)
                    arity None
                else None
              in
              try_builtin_fallback env (fst method_name.ident) arity result)
      | Fetch { base = Var _ | Mem _;
                rev_offset = { o = Dot method_name; _ } :: _ } -> (
          (* For a chained call such as [i.Next.G(s)], the stamp on the bare
             method name resolves the callee. The single-offset branch
             resolves it the same way. The base may also be a dereferenced
             receiver, as in the C and C++ call [p->m(x)]. *)
          match
            signature_via_callee_definition
              ~project_root:env.taint_inst.project_root db
              method_name.id_info arity
          with
          | _ :: _ as found -> found
          | [] ->
              Option.to_list
                (try_builtin_fallback env (fst method_name.ident) arity None))
      | Fetch
          {
            base = VarSpecial ((Self | This | Parent | Super), _);
            rev_offset = [ { o = Dot method_name; _ } ];
          } -> (
          (* A call written [parent::handle($x)] or [super.handle(x)] calls the
             parent class's method on the current object, and one written
             [this.handle(x)] or [self::handle($x)] calls a method of the
             enclosing class on it. The lookup uses the stamp on the bare method
             name, as the self-field branch below does; there is no name-keyed
             database fallback, because a bare method-name lookup would match a
             method of that name on any class. *)
          signature_via_callee_definition
            ~project_root:env.taint_inst.project_root db method_name.id_info
            arity)
      | Fetch
          {
            base = VarSpecial ((Self | This | Parent | Super), _);
            rev_offset = { o = Dot method_name; _ } :: _ :: _;
          } -> (
          (* For a call through a self field such as [self.worker.work(x)],
             where the field takes its type from its initialiser or from its
             callers, the lookup uses the stamp on the bare method name, as the
             chained-variable branch above does. There is no name-keyed
             database fallback, because a bare method-name lookup would match
             a method of that name on any class. *)
          signature_via_callee_definition
            ~project_root:env.taint_inst.project_root db method_name.id_info
            arity)
      | _ -> [])

(* If one of [fun_exp]'s [id_callee_definition] def-site sids is the function
 * currently under analysis, return a synthesised signature built from the effects
 * accumulated so far. The surrounding dataflow fixpoint iterates, so each
 * pass picks up effects recorded by the previous one — converging to a
 * least-fixed-point over direct self-recursion. Both sids derive from the
 * same AST's def token, so the comparison is path-representation-free. *)
let is_self_call env (fun_exp : IL.exp) : bool =
  match (fun_exp.e, env.func.name) with
  | Fetch { base = Var callee; rev_offset = [] }, Some self_name -> (
      List.exists
        (fun (sid : G.SId.t) ->
          (not (G.SId.is_unsafe_default sid))
          && Function_id.equal (Function_id.of_sid sid)
               (Function_id.of_il_name self_name))
        !(callee.id_info.G.id_callee_definition))
  | _ -> false

(* The environment of a closure formed at the current node: a variable
 * captured by reference is the variable itself, one captured by value is
 * its value now. *)
let closure_env (env : env) (sig_ : Signature.t) : S.env =
  sig_.captured
  |> List_.map (fun ((x : IL.name), (mode : AST_generic.capture_mode)) ->
         match mode with
         | Capture_by_reference -> (x, S.Ref { T.base = T.BGlob x; offset = [] })
         | Capture_by_value ->
             ( x,
               S.Val
                 (match Lval_env.find_var env.lval_env x with
                 | Some cell -> cell
                 | None -> S.Cell (`None, S.Bot)) ))

let closure_set_of_definitions (env : env)
    (found : (Function_id.t * Signature.t) list) : S.shape =
  List.fold_left
    (fun (shape : S.shape) (((_, sig_) as definition) : Function_id.t * Signature.t) ->
      Shape.unify_shape ~lang:env.taint_inst.lang shape
        (Shape_and_sig.closure_of_definition definition (closure_env env sig_)))
    S.Bot found

let self_sig_if_recursive env fun_exp =
  match env.func.name with
  | Some self_name when is_self_call env fun_exp ->
      env.did_self_recurse := true;
      Some
        ( Function_id.of_il_name self_name,
          {
            Signature.params = env.func.sig_params;
            params_il = env.func.il_params;
            captured = Lazy.force env.func.captured;
            effects = !(env.effects_acc);
          } )
  | Some _
  | None ->
      None

(* Bound on the offsets composed for a call: one field access on a
   recursive edge (the caller is in a recursive component, or the call is
   a direct self call), [Shape.max_poly_offset] otherwise. See
   [Taint_rule_inst.recursive]. *)
let poly_offset_bound env (fun_exp : IL.exp) : int =
  if env.taint_inst.Taint_rule_inst.recursive || is_self_call env fun_exp
  then Limits_semgrep.taint_MAX_POLY_OFFSET_FLAT
  else Shape.max_poly_offset env.taint_inst.lang

let lookup_signature env fun_exp arity =
  Log.debug (fun m ->
      m "LOOKUP_SIG_ENTRY: Looking up %s with arity %d"
        (Display_IL.string_of_exp fun_exp) arity);
  let found = lookup_signature_with_object_context env fun_exp arity in
  let is_current (def : Function_id.t) : bool =
    match env.func.name with
    | Some self_name -> (
        let self_def = Function_id.of_il_name self_name in
        Function_id.equal def self_def
        ||
        match env.taint_inst.project_root with
        | Some root ->
            Function_id.equal def (Function_id.make_absolute root self_def)
        | None -> false)
    | None -> false
  in
  if
    List.exists
      (fun ((def, _) : Function_id.t * Signature.t) -> is_current def)
      found
  then found
  else found @ Option.to_list (self_sig_if_recursive env fun_exp)

(*****************************************************************************)
(* Lambdas *)
(*****************************************************************************)

let callee_use (callee : IL.exp) : Callee_resolution.callee_use option =
  match callee.e with
  | Fetch { base = Var name; rev_offset = [] } ->
      Callee_resolution.callee_use_of_name name.id_info
  | Fetch { base = Var receiver; rev_offset = [ { o = Dot member; _ } ] } ->
      Callee_resolution.callee_use_of_member ~receiver:receiver.id_info
        (fst member.ident)
  | _ -> None

let argument_types (lang : Lang.t) (args : IL.exp IL.argument list) :
    Callee_resolution.static_type option list =
  List_.map
    (fun (arg : IL.exp IL.argument) ->
      match arg with
      | Unnamed { eorig = SameAs (e : G.expr); _ } ->
          Callee_resolution.static_type_of_argument ~lang e
      | Unnamed _
      | Named _ ->
          None)
    args

let lambdas_used_in_node lambdas node =
  LV.rlvals_of_node node.IL.n |> List_.filter_map (LV.lval_is_lambda lambdas)

let lambdas_used_in_cfg fun_cfg =
  fun_cfg |> LV.reachable_nodes
  |> Seq.fold_left
       (fun used_lambdas_acc node ->
         let lambdas_in_node =
           node
           |> lambdas_used_in_node fun_cfg.lambdas
           |> List.to_seq
           |> Seq.map (fun (lname, _) -> lname)
           |> IL.NameSet.of_seq
         in
         IL.NameSet.union lambdas_in_node used_lambdas_acc)
       IL.NameSet.empty

let lambdas_to_analyze_in_node env lambdas node =
  let unused_lambda_def =
    let* instr =
      match node.F.n with
      | NInstr i -> Some i
      | __else__ -> None
    in
    let* lval = LV.lval_of_instr_opt instr in
    let* ((lname, _) as lambda) = LV.lval_is_lambda lambdas lval in
    if IL.NameSet.mem lname env.func.used_lambdas then None else Some lambda
  in
  Option.to_list unused_lambda_def @ lambdas_used_in_node lambdas node

(* Collect ALL lambdas recursively from a fun_cfg, in innermost-first order.
   This ensures nested lambda signatures are extracted before their parents. *)
let rec collect_all_lambdas_innermost_first (fun_cfg : IL.fun_cfg)
    : (IL.name * IL.fun_cfg) list =
  IL.NameMap.fold (fun name lcfg results ->
    (* First collect nested lambdas from this lambda *)
    let nested = collect_all_lambdas_innermost_first lcfg in
    (* Then add this lambda after its nested ones *)
    results @ nested @ [(name, lcfg)]
  ) fun_cfg.lambdas []

(*****************************************************************************)
(* Miscellaneous *)
(*****************************************************************************)

let check_orig_if_sink env ?filter_sinks orig taints shape =
  (* NOTE(gather-all-taints):
   * A sink is something opaque to us, e.g. consider sink(["ok", "tainted"]),
   * `sink` could potentially access "tainted". So we must take into account
   * all taints reachable through its shape.
   *)
  let taints = taints |> add_taints_from_shape shape in
  let sinks = orig_is_best_sink env orig in
  let sinks =
    match filter_sinks with
    | None -> sinks
    | Some sink_pred -> sinks |> List.filter sink_pred
  in
  let sinks = sinks |> List_.map TM.sink_of_match in
  Log.debug (fun m ->
      let range_str =
        let any =
          match orig with
          | IL.SameAs e -> G.E e
          | IL.Related a -> a
          | IL.NoOrig -> G.Anys []
        in
        match AST_generic_helpers.range_of_any_opt any with
        | Some (s, e) ->
            Printf.sprintf "%d-%d" (s.Tok.pos.bytepos) (e.Tok.pos.bytepos)
        | None -> "?"
      in
      m "SINK_CHECK[range=%s]: taints=%d sinks=%d effects_would_be=%d"
        range_str
        (Taints.cardinal taints) (List.length sinks)
        (if Taints.is_empty taints || List.is_empty sinks then 0 else 1));
  let effects = effects_of_tainted_sinks env taints sinks in
  record_effects env effects

let fix_poly_taint_with_field lang lval xtaint =
  match xtaint with
  | `Sanitized
  | `Clean
  | `None ->
      xtaint
  | `Tainted taints -> (
      match lval.rev_offset with
      | o :: _ ->
          let o = T.offset_of_IL lang o in
          let taints = Shape.fix_poly_taint_with_offset ~lang [ o ] taints in
          `Tainted taints
      | [] -> xtaint)

(*****************************************************************************)
(* Tainted *)
(*****************************************************************************)

let sanitize_lval_by_side_effect lang lval_env sanitizer_pms lval =
  let lval_is_now_safe =
    (* If the l-value is an exact match (overlap > 0.99) for a sanitizer
     * annotation, then we infer that the l-value itself has been updated
     * (presumably by side-effect) and is no longer tainted. We will update
     * the environment (i.e., `lval_env') accordingly. *)
    List.exists
      (fun (m : R.taint_sanitizer TM.t) ->
        m.spec.sanitizer_by_side_effect && TM.is_exact m)
      sanitizer_pms
  in
  if lval_is_now_safe then Lval_env.clean lang lval_env lval else lval_env

(* Check if an expression is sanitized, if so returns `Some' and otherise `None'.
   If the expression is of the form `x.a.b.c` then we try to sanitize it by
   side-effect, in which case this function will return a new lval_env. *)
let exp_is_sanitized env exp =
  match orig_is_best_sanitizer env exp.eorig with
  (* See NOTE [is_sanitizer] *)
  | [] -> None
  | sanitizer_pms -> (
      match exp.e with
      | Fetch lval ->
          Some
            (sanitize_lval_by_side_effect env.taint_inst.lang env.lval_env
               sanitizer_pms lval)
      | __else__ -> Some env.lval_env)

(* Checks if `thing' is a propagator `from' and if so propagates `taints' through it.
   Checks if `thing` is a propagator `'to' and if so fetches any taints that had been
   previously propagated. Returns *only* the newly propagated taint. *)
let handle_taint_propagators env thing taints shape =
  (* We propagate taints via an auxiliary variable (the propagator id). This is
   * simple but it has limitations. It works well to propagate "forward" and,
   * within an instruction node, to propagate in the order in which we visit the
   * subexpressions. E.g. in `x.f(y,z)` we can easily propagate taint from `y` or
   * `z` to `x`, or from `y` to `z`.
   *
   * So, how to propagate taint from `x` to `y` or `z`, or from `z` to `y` ?
   * In Pro, we do it by recording them as "pending" (see
   * 'Taint_lval_env.pending_propagation_dests'). The problem with that kind of
   * "delayed" propagation is that it **only** works by side-effect, but not at
   * the very location of the destination. So we can propagate taint by side-effect
   * from `z` to `y` in `x.f(y,z)`, but the `y` occurrence that is the actual
   * destination (i.e. the `$TO`) will not have the taints coming from `z`, only
   * the subsequent occurrences of `y` will.
   * TODO: To support that, we may need to introduce taint variables that we can
   *       later substitute, like we do for labels.
   *)
  let taints = taints |> add_taints_from_shape shape in
  let lval_env = env.lval_env in
  let propagators =
    let any =
      match thing with
      | `Lval lval -> any_of_lval lval
      | `Exp exp -> any_of_orig exp.eorig
      | `Ins ins -> any_of_orig ins.iorig
    in
    env.taint_inst.preds.is_propagator any
  in
  let propagate_froms, propagate_tos =
    List.partition (fun p -> p.TM.spec.TRI.kind =*= `From) propagators
  in
  let lval_env =
    (* `thing` is the source (the "from") of propagation, we add its taints to
     * the environment. *)
    List.fold_left
      (fun lval_env prop ->
        (* Only propagate if the current set of taint labels can satisfy the
           propagator's requires precondition.
        *)
        (* TODO(brandon): Interprocedural propagator labels
           This is trickier than I thought. You have to augment the Arg taints
           with preconditions as well, and allow conjunction, because when you
           replace an Arg taint with a precondition, all the produced taints
           inherit the precondition. There's not an easy way to express this
           in the type right now.

           More concretely, the existence of labeled propagators means that
           preconditions can be attached to arbitrary taint. This is because
           if we have a taint that is being propagated with a `requires`, then
           that taint now has a precondition on that `requires` being true. This
           taint might also be an `Arg` taint, meaning that `Arg` taints can
           have preconditions.

           This is more than just a simple type-level change because when `Arg`s
           have preconditions, what happens for substitution? Say I want to
           replace an `Arg x` taint with [t], that is, a single taint. Well,
           that taint `t` might itself have a precondition. That means that we
           now have a taint which is `t`, substituted for `Arg x`, but also
           inheriting `Arg x`'s precondition. Our type for preconditions doesn't
           allow arbitrary conjunction of preconditions like that, so this is
           more pervasive of a change.

           I'll come back to this later.
        *)
        match
          T.solve_precondition ~ignore_poly_taint:false ~taints
            (R.get_propagator_precondition prop.TM.spec.TRI.prop)
        with
        | Some true ->
            (* If we have an output label, change the incoming taints to be
               of the new label.
               Otherwise, keep them the same.
            *)
            let new_taints =
              match prop.TM.spec.prop.propagator_label with
              | None -> taints
              | Some label ->
                  (* Relabeling changes taint identity, so re-key the set. *)
                  Taints.map_taint
                    (propagate_taint_to_label
                       prop.spec.prop.propagator_replace_labels label)
                    taints
            in
            Lval_env.propagate_to env.taint_inst.lang prop.spec.var new_taints
              lval_env
        | Some false
        | None ->
            lval_env)
      lval_env propagate_froms
  in
  let taints_propagated, lval_env =
    (* `thing` is the destination (the "to") of propagation. we collect all the
     * incoming taints by looking for the propagator ids in the environment. *)
    List.fold_left
      (fun (taints_in_acc, lval_env) prop ->
        let opt_propagated, lval_env =
          Lval_env.propagate_from prop.TM.spec.TRI.var lval_env
        in
        let taints_from_prop =
          match opt_propagated with
          | None -> Taints.empty
          | Some taints -> taints
        in
        let lval_env =
          if prop.spec.TRI.prop.propagator_by_side_effect then
            match thing with
            (* If `thing` is an l-value of the form `x.a.b.c`, then taint can be
             *  propagated by side-effect. A pattern-propagator may use this to
             * e.g. propagate taint from `x` to `y` in `f(x,y)`, so that
             * subsequent uses of `y` are tainted if `x` was previously tainted. *)
            | `Lval lval ->
                if Option.is_some opt_propagated then
                  lval_env
                  |> Lval_env.add_lval env.taint_inst.lang lval
                       taints_from_prop
                else
                  (* If we did not find any taint to be propagated, it could
                   * be because we have not encountered the 'from' yet, so we
                   * add the 'lval' to a "pending" queue. *)
                  lval_env |> Lval_env.pending_propagation prop.TM.spec.var lval
            | `Exp _
            | `Ins _ ->
                lval_env
          else lval_env
        in
        (Taints.union taints_in_acc taints_from_prop, lval_env))
      (Taints.empty, lval_env) propagate_tos
  in
  (taints_propagated, lval_env)

let find_lval_taint_sources env incoming_taints lval =
  let taints_of_pms env = taints_of_matches env ~incoming:incoming_taints in
  let source_pms = lval_is_source env lval in
  (* Partition sources according to the value of `by-side-effect:`,
   * either `only`, `yes`, or `no`. *)
  let ( `Only by_side_effect_only_pms,
        `Yes by_side_effect_yes_pms,
        `No by_side_effect_no_pms ) =
    partition_sources_by_side_effect source_pms
  in
  let by_side_effect_only_taints, lval_env =
    by_side_effect_only_pms
    (* We require an exact match for `by-side-effect` to take effect. *)
    |> List.filter TM.is_exact
    |> taints_of_pms env
  in
  let by_side_effect_yes_taints, lval_env =
    by_side_effect_yes_pms
    (* We require an exact match for `by-side-effect` to take effect. *)
    |> List.filter TM.is_exact
    |> taints_of_pms { env with lval_env }
  in
  let by_side_effect_no_taints, lval_env =
    by_side_effect_no_pms |> taints_of_pms { env with lval_env }
  in
  let taints_to_add_to_env =
    by_side_effect_only_taints |> Taints.union by_side_effect_yes_taints
  in
  let lval_env =
    lval_env |> Lval_env.add_lval env.taint_inst.lang lval taints_to_add_to_env
  in
  let taints_to_return =
    Taints.union by_side_effect_no_taints by_side_effect_yes_taints
  in
  (taints_to_return, lval_env)

let rec check_tainted_lval env (lval : IL.lval) :
    Taints.t * S.shape * [ `Sub of Taints.t * S.shape ] * Lval_env.t =
  let new_taints, lval_in_env, lval_shape, sub, lval_env =
    check_tainted_lval_aux env lval
  in
  let taints_from_env = Xtaint.to_taints lval_in_env in
  let taints = Taints.union new_taints taints_from_env in
  let taints =
    check_type_and_drop_taints_if_bool_or_number env taints type_of_lval lval
  in
  let sinks =
    lval_is_sink env lval
    |> List.filter (TM.is_best_match env.func.best_matches)
    |> List_.map TM.sink_of_match
  in
  if (not (Taints.is_empty taints)) && not (List.is_empty sinks) then ();
  let effects = effects_of_tainted_sinks { env with lval_env } taints sinks in
  record_effects { env with lval_env } effects;
  (taints, lval_shape, sub, lval_env)

(* Java: Whenever we find a getter/setter without definition we end up here,
 * this happens if the getter/setters are being autogenerated at build time,
 * as when you use Lombok. This function will "resolve" the getter/setter to
 * the corresponding property, and propagate taint to/from that property.
 * So that `o.getX()` returns whatever taints `o.x` has, and so `o.setX(E)`
 * propagates any taints in `E` to `o.x`. *)
and propagate_taint_via_java_getters_and_setters_without_definition env e args
    all_args_taints =
  match e with
  | {
   e =
     Fetch
       ({
          base = Var _obj;
          rev_offset =
            [ { o = Dot { IL.ident = method_str, method_tok; sid; _ }; _ } ];
        } as lval);
   _;
  }
  (* We check for the "get"/"set" prefix below. *)
    when env.taint_inst.lang =*= Lang.Java && String.length method_str > 3 ->
      begin
        let mk_prop_lval () =
          (* e.g. getFooBar/setFooBar -> fooBar *)
          let prop_str =
          String.uncapitalize_ascii (Str.string_after method_str 3)
          in
          let prop_name =
          match
              Hashtbl.find_opt env.taint_inst.java_props_cache (prop_str, sid)
          with
          | Some prop_name -> prop_name
          | None ->
              let prop_name =
                {
                  ident = (prop_str, method_tok);
                  sid = G.SId.unsafe_default;
                  id_info = G.empty_id_info ();
                }
              in
              Hashtbl.add env.taint_inst.java_props_cache (prop_str, sid)
                prop_name;
              prop_name
          in
          { lval with rev_offset = [ { o = Dot prop_name; oorig = NoOrig } ] }
        in
        match args with
        | [] when String.(starts_with ~prefix:"get" method_str) ->
            let taints, shape, _sub, lval_env =
                check_tainted_lval env (mk_prop_lval ())
            in
            Some (taints, shape, lval_env)
        | [ _ ] when String.starts_with ~prefix:"set" method_str ->
            if not (Taints.is_empty all_args_taints) then
                Some
                ( Taints.empty,
                    Bot,
                    env.lval_env
                    |> Lval_env.add_lval env.taint_inst.lang (mk_prop_lval ())
                         all_args_taints )
            else Some (Taints.empty, Bot, env.lval_env)
        | __else__ -> None
      end
  | __else__ -> None

and check_tainted_lval_aux env (lval : IL.lval) :
    Taints.t
    * Xtaint.t_or_sanitized
    * S.shape
    * [ `Sub of Taints.t * S.shape ]
    * Lval_env.t =
  (* Recursively checks an l-value bottom-up.
   *
   *  This check needs to combine matches from pattern-{sources,sanitizers,sinks}
   *  with the info we have stored in `env.lval_env`. This can be subtle, see
   *  comments below.
   *)
  match lval_is_best_sanitizer env lval with
  (* See NOTE [is_sanitizer] *)
  (* TODO: We should check that taint and sanitizer(s) are unifiable. *)
  | _ :: _ as sanitizer_pms ->
      (* NOTE [lval/sanitized]:
       *  If lval is sanitized, then we will "bubble up" the `Sanitized status, so
       *  any taint recorded in lval_env for any extension of lval will be discarded.
       *
       *  So, if we are checking `x.a.b.c` and `x.a` is sanitized then any extension
       *  of `x.a` is considered sanitized as well, and we do look for taint info in
       *  the environment.
       *
       *  *IF* sanitization is side-effectful then any taint info will be removed
       *  from lval_env by sanitize_lval, but that is not guaranteed.
       *)
      let lval_env =
        sanitize_lval_by_side_effect env.taint_inst.lang env.lval_env
          sanitizer_pms lval
      in
      (Taints.empty, `Sanitized, Bot, `Sub (Taints.empty, Bot), lval_env)
  | [] ->
      (* Recursive call, check sub-lvalues first.
       *
       * It needs to be done bottom-up because any sub-lvalue can be a source and a
       * sink by itself, even if an extension of lval is not. For example, given
       * `x.a.b`, this lvalue may be considered sanitized, but at the same time `x.a`
       * could be tainted and considered a sink in some context. We cannot just check
       * `x.a.b` and forget about the sub-lvalues.
       *)
      let sub_new_taints, sub_in_env, sub_shape, lval_env =
        match lval with
        | { base; rev_offset = [] } ->
            (* Base case, no offset. *)
            check_tainted_lval_base env base
        | { base = _; rev_offset = _ :: rev_offset' } ->
            (* Recursive case, given `x.a.b` we must first check `x.a`. *)
            let sub_new_taints, sub_in_env, sub_shape, _sub_sub, lval_env =
              check_tainted_lval_aux env { lval with rev_offset = rev_offset' }
            in
            (sub_new_taints, sub_in_env, sub_shape, lval_env)
      in
      let sub_new_taints, sub_in_env =
        if env.taint_inst.options.taint_only_propagate_through_assignments then
          match sub_in_env with
          | `Sanitized -> (Taints.empty, `Sanitized)
          | `Clean
          | `None
          | `Tainted _ ->
              (Taints.empty, `None)
        else (sub_new_taints, sub_in_env)
      in
      (* Check the status of lval in the environemnt. *)
      let lval_in_env, lval_shape =
        match sub_in_env with
        | `Sanitized ->
            (* See NOTE [lval/sanitized] *)
            (`Sanitized, S.Bot)
        | (`Clean | `None | `Tainted _) as sub_xtaint ->
            let xtaint', shape =
              (* THINK: Should we just use 'Sig.find_in_shape' directly here ?
                       We have the 'sub_shape' available. *)
              match Lval_env.find_lval env.taint_inst.lang lval_env lval with
              | None -> (`None, S.Bot)
              | Some (Cell (xtaint', shape)) -> (xtaint', shape)
            in
            let xtaint' =
              match xtaint' with
              | (`Clean | `Tainted _) as xtaint' -> xtaint'
              | `None ->
                  (* HACK(field-sensitivity): If we encounter `obj.x` and `obj` has
                   * polymorphic taint, and we know nothing specific about `obj.x`, then
                   * we add the same offset `.x` to the polymorphic taint coming from `obj`.
                   * (See also 'propagate_taint_via_unresolved_java_getters_and_setters'.)
                   *
                   * For example, given `function foo(o) { sink(o.x); }`, and being '0 the
                   * polymorphic taint of `o`, this allows us to record that what goes into
                   * the sink is '0.x (and not just '0). So if later we encounter `foo(obj)`
                   * where `obj.y` is tainted but `obj.x` is not tainted, we will not
                   * produce a finding.
                   *)
                  fix_poly_taint_with_field env.taint_inst.lang lval sub_xtaint
            in
            (xtaint', shape)
      in
      let taints_from_env = Xtaint.to_taints lval_in_env in
      (* Find taint sources matching lval. *)
      let current_taints = Taints.union sub_new_taints taints_from_env in
      let taints_from_sources, lval_env =
        find_lval_taint_sources { env with lval_env } current_taints lval
      in
      (* Check sub-expressions in the offset. *)
      let taints_from_offset, lval_env =
        match lval.rev_offset with
        | [] -> (Taints.empty, lval_env)
        | offset :: _ -> check_tainted_lval_offset { env with lval_env } offset
      in
      (* Check taint propagators. *)
      let taints_incoming (* TODO: find a better name *) =
        if env.taint_inst.options.taint_only_propagate_through_assignments then
          taints_from_sources
        else
          sub_new_taints
          |> Taints.union taints_from_sources
          |> Taints.union taints_from_offset
      in
      let taints_propagated, lval_env =
        handle_taint_propagators { env with lval_env } (`Lval lval)
          (taints_incoming |> Taints.union taints_from_env)
          lval_shape
      in
      let new_taints = taints_incoming |> Taints.union taints_propagated in
      let sinks =
        lval_is_sink env lval
        (* For sub-lvals we require sinks to be exact matches. Why? Let's say
         * we have `sink(x.a)` and `x' is tainted but `x.a` is clean...
         * with the normal subset semantics for sinks we would consider `x'
         * itself to be a sink, and we would report a finding!
         *)
        |> List.filter TM.is_exact
        |> List_.map TM.sink_of_match
      in
      let all_taints = Taints.union taints_from_env new_taints in
      let effects =
        effects_of_tainted_sinks { env with lval_env } all_taints sinks
      in
      record_effects { env with lval_env } effects;
      ( new_taints,
        lval_in_env,
        lval_shape,
        `Sub (Xtaint.to_taints sub_in_env, sub_shape),
        lval_env )

and check_tainted_lval_base env base =
  match base with
  | Var _
  | VarSpecial _ ->
      (Taints.empty, `None, Bot, env.lval_env)
  | Mem { e = Fetch lval; _ } ->
      (* i.e. `*ptr` *)
      let taints, lval_in_env, shape, _sub, lval_env =
        check_tainted_lval_aux env lval
      in
      (taints, lval_in_env, shape, lval_env)
  | Mem e ->
      let taints, shape, lval_env = check_tainted_expr env e in
      (taints, `None, shape, lval_env)

and check_tainted_lval_offset env offset =
  match offset.o with
  | Dot _n ->
      (* THINK: Allow fields to be taint sources, sanitizers, or sinks ??? *)
      (Taints.empty, env.lval_env)
  | Index e ->
      let taints, _shape, lval_env = check_tainted_expr env e in
      let taints =
        if propagate_through_indexes env then taints
        else (* Taints from the index should be ignored. *)
          Taints.empty
      in
      (taints, lval_env)
  | Slice _ ->
      (* The slice index is a static int, no expression to check. *)
      (Taints.empty, env.lval_env)

(* Test whether an expression is tainted, and if it is also a sink,
 * report the finding too (by side effect). *)
and check_tainted_expr ?(arity = 0) env exp : Taints.t * S.shape * Lval_env.t =
  let check env = check_tainted_expr env in
  let check_subexpr exp =
    match exp.e with
    | Fetch _
    (* TODO: 'Fetch' is handled specially, this case should not never be taken.  *)
    | Literal _
    | FixmeExp (_, _, None) ->
        (Taints.empty, S.Bot, env.lval_env)
    | FixmeExp (_, _, Some e) ->
        let taints, shape, lval_env = check env e in
        let taints = taints |> add_taints_from_shape shape in
        (taints, S.Bot, lval_env)
    | Composite ((CTuple | CArray | CList), (_, es, _)) ->
        let taints_and_shapes, lval_env = map_check_expr env check es in
        let tuple_shape = Shape.tuple_like_obj taints_and_shapes in
        let all_taints =
          taints_and_shapes
          |> List.fold_left
               (fun acc (taints, shape) ->
                 acc |> Taints.union taints |> add_taints_from_shape shape)
               Taints.empty
        in
        (all_taints, tuple_shape, lval_env)
    | Composite ((CSet | Constructor _ | Regexp), (_, es, _)) ->
        let taints, lval_env = union_map_taints_and_vars env check es in
        (taints, S.Bot, lval_env)
    | Operator ((op, _), es) ->
        let args_taints, all_args_taints, lval_env =
          check_function_call_arguments env es
        in
        let all_args_taints =
          all_args_taints
          |> Taints.union (gather_all_taints_in_args_taints args_taints)
        in
        let all_args_taints =
          if env.taint_inst.options.taint_only_propagate_through_assignments
          then Taints.empty
          else all_args_taints
        in
        let op_taints =
          match op with
          | G.Eq
          | G.NotEq
          | G.PhysEq
          | G.NotPhysEq
          | G.Lt
          | G.LtE
          | G.Gt
          | G.GtE
          | G.Cmp
          | G.RegexpMatch
          | G.NotMatch
          | G.In
          | G.NotIn
          | G.Is
          | G.NotIs ->
              if env.taint_inst.options.taint_assume_safe_comparisons then
                Taints.empty
              else all_args_taints
          | G.And
          | G.Or
          | G.Xor
          | G.Not
          | G.LSL
          | G.LSR
          | G.ASR
          | G.BitOr
          | G.BitXor
          | G.BitAnd
          | G.BitNot
          | G.BitClear
          | G.Plus
          | G.Minus
          | G.Mult
          | G.Div
          | G.Mod
          | G.Pow
          | G.FloorDiv
          | G.MatMult
          | G.Concat
          | G.Append
          | G.Range
          | G.RangeInclusive
          | G.NotNullPostfix
          | G.Length
          | G.Elvis
          | G.Nullish
          | G.Background
          | G.Pipe
          | G.LDA
          | G.RDA
          | G.LSA
          | G.RSA ->
              all_args_taints
        in
        (op_taints, S.Bot, lval_env)
    | RecordOrDict fields ->
        (* TODO: Construct a proper record/dict shape here. *)
        let (lval_env, taints), taints_and_shapes =
          fields
          |> List.fold_left_map
               (fun (lval_env, taints_acc) field ->
                 match field with
                 | Field (id, e) ->
                     (* TODO: Check 'id' for taint? *)
                     let e_taints, e_shape, lval_env =
                       check { env with lval_env } e
                     in
                     let taints_acc =
                       taints_acc |> Taints.union e_taints
                       |> add_taints_from_shape e_shape
                     in
                     ((lval_env, taints_acc), `Field (id, e_taints, e_shape))
                 | Spread e ->
                     let e_taints, e_shape, lval_env =
                       check { env with lval_env } e
                     in
                     let taints_acc =
                       taints_acc |> Taints.union e_taints
                       |> add_taints_from_shape e_shape
                     in
                     ((lval_env, taints_acc), `Spread e_shape)
                 | Entry (ke, ve) ->
                     let ke_taints, ke_shape, lval_env =
                       check { env with lval_env } ke
                     in
                     let taints_acc =
                       taints_acc |> Taints.union ke_taints
                       |> add_taints_from_shape ke_shape
                     in
                     let ve_taints, ve_shape, lval_env =
                       check { env with lval_env } ve
                     in
                     let taints_acc =
                       taints_acc
                       |> Taints.union
                            ve_taints (* ← Now includes value taints! *)
                       |> add_taints_from_shape ve_shape
                     in
                     ((lval_env, taints_acc), `Entry (ke, ve_taints, ve_shape)))
               (env.lval_env, Taints.empty)
        in
        let record_shape =
          Shape.record_or_dict_like_obj ~lang:env.taint_inst.lang
            taints_and_shapes
        in
        (taints, record_shape, lval_env)
    | Cast (_, e) -> check env e
  in
  match exp_is_sanitized env exp with
  (* THINK: Can we just skip checking the subexprs in 'exp'? There could be a
   * sanitizer by-side-effect that will not trigger, see CODE-6548. E.g.
   * if `x` in `foo(x)` is supposed to be sanitized by-side-effect, but `foo(x)`
   * itself is sanitized, the by-side-effect sanitization of `x` will not happen.
   * Problem is, we do not want sources or propagators by-side-effect to trigger
   * on `x` if `foo(x)` is sanitized, so we would need to check the subexprs while
   * disabling taint sources.
   *)
  | Some lval_env ->
      (* TODO: We should check that taint and sanitizer(s) are unifiable. *)
      (Taints.empty, Bot, lval_env)
  | None ->
      let taints, shape, lval_env =
        match exp.e with
        | Fetch lval ->
            let taints, shape, _sub, lval_env = check_tainted_lval env lval in
            Log.debug (fun m ->
                m "FETCH_CHECK[fn=%s]: %s -> taints=%d shape=%s"
                  (match env.func.name with
                  | Some n -> fst n.IL.ident
                  | None -> "?")
                  (Display_IL.string_of_exp exp) (Taints.cardinal taints)
                  (S.show_shape shape));
            (* Give fn-references a [Fun] shape so HOF callback dispatch finds a signature; keep existing [S.Fun] shapes. *)
            let shape =
              match shape with
              | S.Fun _ -> shape
              | _ ->
                  let is_temp_var =
                    match lval.base with
                    | Var name -> String.starts_with ~prefix:"_tmp" (fst name.ident)
                    | _ -> false
                  in
                  if is_temp_var then shape
                  else
                    (match lookup_signature env exp arity with
                    | [] -> shape
                    | found -> closure_set_of_definitions env found)
            in
            (taints, shape, lval_env)
        | __else__ ->
            let taints_exp, shape, lval_env = check_subexpr exp in
            let taints_sources, lval_env =
              orig_is_best_source env exp.eorig
              |> taints_of_matches { env with lval_env } ~incoming:taints_exp
            in
            let taints = taints_exp |> Taints.union taints_sources in
            let taints_propagated, lval_env =
              handle_taint_propagators { env with lval_env } (`Exp exp) taints
                shape
            in
            let taints = Taints.union taints taints_propagated in
            (taints, shape, lval_env)
      in
      check_orig_if_sink env exp.eorig taints shape;
      (taints, shape, lval_env)

(* Check the actual arguments of a function call. This also handles left-to-right
 * taint propagation by chaining the 'lval_env's returned when checking the arguments.
 * For example, given `foo(x.a)` we'll check whether `x.a` is tainted or whether the
 * argument is a sink. *)
and check_function_call_arguments env args =
  let (rev_taints, lval_env), args_taints =
    args
    |> List.fold_left_map
         (fun (rev_taints, lval_env) arg ->
           let e = IL_helpers.exp_of_arg arg in
           let taints, shape, lval_env =
             check_tainted_expr { env with lval_env } e
           in
           Log.debug (fun m ->
               m "CHECK_ARGS: exp=%s -> taints=%d shape=%s"
                 (Display_IL.string_of_exp e) (Taints.cardinal taints)
                 (S.show_shape shape));
           let taints =
             check_type_and_drop_taints_if_bool_or_number env taints
               type_of_expr e
           in
           let new_acc = (taints :: rev_taints, lval_env) in
           match arg with
           | Unnamed _ -> (new_acc, Unnamed (taints, shape))
           | Named (id, _) -> (new_acc, Named (id, (taints, shape))))
         ([], env.lval_env)
  in
  let all_args_taints = List.fold_left Taints.union Taints.empty rev_taints in
  (args_taints, all_args_taints, lval_env)

let check_tainted_var env (var : IL.name) : Taints.t * S.shape * Lval_env.t =
  let taints, shape, _sub, lval_env =
    check_tainted_lval env (LV.lval_of_var var)
  in
  (taints, shape, lval_env)

(* A [Sig_inst.call_effect.ToSinkInCall] bubbling out of instantiation means
 * the callback's signature could not be resolved inside [Sig_inst]. This
 * helper gives it one more chance at the use site by name-looking-up the
 * saved [callee] expression, and — on success — consumes the callback's
 * resolved effects (recording sinks, propagating return taints/shapes, and
 * applying lval updates) against the fold's running accumulator. On any
 * failure it re-records the preserved effect so a caller one level up can
 * try again.
 *
 * Nested [ToSinkInCall] effects returned by the resolver are re-recorded
 * with their own [callee/arg/arg_offset/args_taints]; the outer preserved
 * effect is only re-recorded when resolution fails outright. *)
let resolve_preserved_to_sink_in_call env ~callee ~arg ~arg_offset
    ~args_taints ~rebound_guards (taints_acc, shape_acc, lval_env) =
  let resolved_call_effects =
    try
      let callee_name_opt =
        match callee.e with
        | Fetch { base = Var name; rev_offset = [] }
        | Fetch { base = Var name; rev_offset = [ { o = Dot _; _ } ] } ->
            Some name
        | _ -> None
      in
      match callee_name_opt with
      | Some callee_name ->
          let arity = List.length args_taints in
          (match lookup_signature env callee arity with
          | _ :: _ as found ->
              Log.debug (fun m ->
                  m "Resolving ToSinkInCall for '%s' at use site"
                    (IL.str_of_name callee_name));
              Some
                (List.concat_map
                   (fun ((_, callee_sig) : Function_id.t * Signature.t) ->
                     Sig_inst.instantiate_function_signature
                       ~lang:env.taint_inst.lang
                       ~atoms:env.shared_tables.guard_atoms
                       ~max_offset:(poly_offset_bound env callee)
                       ~outer_params:env.func.il_params
                       env.lval_env callee_sig ~callee ~args:None args_taints
                       ~lookup_sig:(fun exp _depth ->
                         let arity = List.length args_taints in
                         lookup_signature env exp arity)
                       ())
                   found)
          | [] ->
              Log.debug (fun m ->
                  m "ToSinkInCall: No signature found for '%s'"
                    (IL.str_of_name callee_name));
              None)
      | None ->
          Log.debug (fun m ->
              m "ToSinkInCall: Could not resolve callee '%s'"
                (Display_IL.string_of_exp callee));
          None
    with e ->
      Log.warn (fun m ->
          m "Exception while resolving ToSinkInCall: %s" (Common.exn_to_s e));
      None
  in
  match resolved_call_effects with
  | Some resolved_effects ->
      List.fold_left
        (fun (taints_acc, shape_acc, lval_env)
             (resolved_effect : Sig_inst.call_effect) ->
          match resolved_effect with
          | ToSink
              {
                taints_with_precondition = incoming_taints, requires;
                sink;
                guards = inner_guards;
                _;
              } ->
              let combined_guards =
                Effect_guard.compose_and rebound_guards inner_guards
              in
              let sink_effects =
                effects_of_tainted_sink env incoming_taints sink
              in
              let corrected_sink_effects =
                sink_effects
                |> List.map (function
                     | Effect.ToSink eff ->
                         Effect.ToSink
                           {
                             eff with
                             taints_with_precondition =
                               (fst eff.taints_with_precondition, requires);
                             guards =
                               Effect_guard.compose_and eff.guards
                                 combined_guards;
                           }
                     | other -> other)
              in
              record_effects env corrected_sink_effects;
              (taints_acc, shape_acc, lval_env)
          | ToReturn
              {
                data_taints = taints;
                data_shape = shape;
                control_taints;
                guards = inner_guards;
                _;
              } ->
              (* Gate the returned taints by the guards, like the sibling
                 [ToSink]/[ToSinkInCall] arms above: [rebound_guards] is the
                 guard on the [ToSinkInCall] being resolved (it gates every
                 effect from this resolution) and [inner_guards] is the guard
                 on the resolved return. The main-handler [ToReturn] arm in
                 [check_function_call] conjoins [inner_guards] the same way. *)
              let taints =
                Taints.conjoin_guard
                  (Effect_guard.compose_and rebound_guards inner_guards)
                  taints
              in
              ( Taints.union taints taints_acc,
                Shape.unify_shape ~lang:env.taint_inst.lang shape shape_acc,
                Lval_env.add_control_taints lval_env control_taints )
          | ToLval { taints; var; offset; guards } ->
              if not (is_own_param env var) then
                record_effects env
                  [ Effect.ToLval
                      { taints;
                        lval = { base = base_of_free_var env var; offset };
                        guards } ];
              (* As in the main [ToLval] arm: the written taints carry the
               * guard, conjoined with [rebound_guards] like the sibling
               * arms of this resolution. *)
              let taints =
                Taints.conjoin_guard
                  (Effect_guard.compose_and rebound_guards guards)
                  taints
              in
              ( taints_acc,
                shape_acc,
                lval_env
                |> Lval_env.add_written_through env.taint_inst.lang var offset taints )
          | ToLvalThis { taints; offset; guards } ->
              let guards = Effect_guard.compose_and rebound_guards guards in
              record_this_field_write env taints offset guards;
              (* Mirror the sibling [ToLval] arm: conjoin the guard, then also
                 reflect the write in the local [lval_env]. *)
              let taints = Taints.conjoin_guard guards taints in
              ( taints_acc,
                shape_acc,
                add_this_field_to_lval_env env lval_env offset taints )
          | ToSinkInCall
              {
                callee;
                arg;
                arg_offset;
                args_taints;
                guards = inner_guards;
              } ->
              record_effects env
                [
                  Effect.ToSinkInCall
                    {
                      callee;
                      arg;
                      arg_offset;
                      args_taints;
                      guards =
                        Effect_guard.compose_and rebound_guards inner_guards;
                    };
                ];
              (taints_acc, shape_acc, lval_env))
        (taints_acc, shape_acc, lval_env)
        resolved_effects
  | None ->
      record_effects env
        [
          Effect.ToSinkInCall
            {
              callee;
              arg;
              arg_offset;
              args_taints;
              guards = rebound_guards;
            };
        ];
      (taints_acc, shape_acc, lval_env)

(* This function is consuming the taint signature of a function to determine
   a few things:
   1) What is the status of taint in the current environment, after the function
      call occurs?
   2) Are there any effects that occur within the function due to taints being
      input into the function body, from the calling context?
*)
let check_function_call env fun_exp args
    (args_taints : (Taints.t * S.shape) argument list) () :
    (Taints.t * S.shape * Lval_env.t) option =
  let arity = List.length args in
  Log.debug (fun m ->
      m "CHECK_FUNCTION_CALL: %s with arity %d, intrafile=%b"
        (Display_IL.string_of_exp fun_exp) arity
        env.taint_inst.options.taint_intrafile);
  let sig_result =
    if env.taint_inst.options.taint_intrafile then
      (* The callee's Fun shape in lval_env comes before a signature found by
       * name. This handles two cases:
       *   callback(source())       -- direct call, lval = callback
       *   callback.run(source())   -- invoke method, lval = callback.run
       * For invoke methods (e.g. Java Runnable.run), strip the method offset
       * and look up the base variable. *)
      let lval_to_check =
        match fun_exp.e with
        | Fetch lval -> (
            let invoke_methods =
              (Lang_config.get env.taint_inst.lang).invoke_methods
            in
            match lval.rev_offset with
            | [ { o = Dot method_name; _ } ]
              when List.mem (fst method_name.ident) invoke_methods ->
                Some { lval with rev_offset = [] }
            | _ -> Some lval)
        | _ -> None
      in
      let from_shape =
        let* lval_to_check = lval_to_check in
        match
          Lval_env.find_lval env.taint_inst.lang env.lval_env lval_to_check
        with
        | Some (S.Cell (_, S.Fun (c, cs))) ->
            Log.debug (fun m ->
                m "SIG_FROM_SHAPE: Found Fun shape for %s"
                  (Display_IL.string_of_exp fun_exp));
            Some
              (List_.map
                 (fun (closure : S.closure) ->
                   (closure.sig_, Some closure.env))
                 (c :: cs))
        | _ -> None
      in
      match from_shape with
      | Some _ -> from_shape
      | None -> (
          match lookup_signature env fun_exp arity with
          | _ :: _ as found ->
              Some
                (List_.map
                   (fun ((_, fun_sig) : Function_id.t * Signature.t) ->
                     (fun_sig, None))
                   found)
          | [] -> (
              (* Sym-prop fallback: if the variable's [id_svalue] resolves
               * to a bare function reference (e.g. [cb = handler]), look
               * up the referenced function's signature in the DB. *)
              match lval_to_check with
              | Some { base = Var x; rev_offset = [] } -> (
                  match !(x.id_info.id_svalue) with
                  | Some
                      (G.Sym
                         {
                           e = G.N (G.Id (ident, id_info));
                           _;
                         }) ->
                      let il_name =
                        AST_to_IL.var_of_id_info ident id_info
                      in
                      let aliased_exp =
                        {
                          IL.e =
                            IL.Fetch
                              {
                                base = IL.Var il_name;
                                rev_offset = [];
                              };
                          eorig = IL.NoOrig;
                        }
                      in
                      Log.debug (fun m ->
                          m
                            "SIG_FROM_SVALUE: var=%s resolves to %s"
                            (IL.str_of_name x)
                            (IL.str_of_name il_name));
                      (match lookup_signature env aliased_exp arity with
                      | [] -> None
                      | found ->
                          Some
                            (List_.map
                               (fun ((_, fun_sig) : Function_id.t * Signature.t) ->
                                 (fun_sig, None))
                               found))
                  | _ -> None)
              | _ -> None))
    else None
  in
  match sig_result with
  | Some members ->
      (* Callback lookup in both modes; effects-explosion hazard contained by [Sig_inst.preserve_effect]. *)
      let call_effects =
        members
        |> List.concat_map (fun (fun_sig, fun_env) ->
               Log.debug (fun m ->
                   m "SIG_FOUND: %s -> %s"
                     (Display_IL.string_of_exp fun_exp)
                     (Signature.show fun_sig));
               Sig_inst.instantiate_function_signature
                 ~lang:env.taint_inst.lang
                 ~atoms:env.shared_tables.guard_atoms
                 ~max_offset:(poly_offset_bound env fun_exp)
                 ~outer_params:env.func.il_params ?env:fun_env env.lval_env
                 fun_sig ~callee:fun_exp ~args:(Some args) args_taints
                 ~lookup_sig:(lookup_signature env) ())
      in
      Log.debug (fun m ->
          m "INSTANTIATE_SIG: %s returned %d call_effects"
            (Display_IL.string_of_exp fun_exp)
            (List.length call_effects));
      List.iteri (fun i eff ->
        match eff with
        | Sig_inst.ToReturn { data_taints; _ } ->
            Log.debug (fun m ->
                m "INSTANTIATE_SIG: Effect[%d] ToReturn with %d taints: %s"
                  i
                  (Taint.Taint_set.cardinal data_taints)
                  (Taint.show_taints data_taints))
        | Sig_inst.ToSink { taints_with_precondition = (taints, _); _ } ->
            Log.debug (fun m ->
                m "INSTANTIATE_SIG: Effect[%d] ToSink with %d taint items"
                  i
                  (List.length taints))
        | Sig_inst.ToLval { taints; _ } ->
            Log.debug (fun m ->
                m "INSTANTIATE_SIG: Effect[%d] ToLval with %d taints"
                  i
                  (Taint.Taint_set.cardinal taints))
        | Sig_inst.ToLvalThis { taints; _ } ->
            Log.debug (fun m ->
                m "INSTANTIATE_SIG: Effect[%d] ToLvalThis with %d taints"
                  i
                  (Taint.Taint_set.cardinal taints))
        | Sig_inst.ToSinkInCall { args_taints; arg; arg_offset; _ } ->
            Log.debug (fun m ->
                m "INSTANTIATE_SIG: Effect[%d] ToSinkInCall arg=%s offset=%s args=%s"
                  i (T.show_formal arg) (T.show_offset_list arg_offset)
                  (Effect.show_args_taints args_taints))
      ) call_effects;
      Some
        (call_effects
        |> List.fold_left
             (fun (taints_acc, shape_acc, lval_env)
                  (call_effect : Sig_inst.call_effect) ->
               match call_effect with
               | ToSink
                   {
                     taints_with_precondition = incoming_taints, requires;
                     sink;
                     guards = rebound_guards;
                     _;
                   } ->
                   (* Call effects_of_tainted_sink to get proper taint traces, then fix the requires condition *)
                   let sink_effects =
                     effects_of_tainted_sink env incoming_taints sink
                   in
                   let corrected_sink_effects =
                     sink_effects
                     |> List.map (function
                          | Effect.ToSink eff ->
                              Effect.ToSink
                                {
                                  eff with
                                  taints_with_precondition =
                                    (fst eff.taints_with_precondition, requires);
                                  guards =
                                    Effect_guard.compose_and eff.guards
                                      rebound_guards;
                                }
                          | other -> other)
                   in
                   record_effects env corrected_sink_effects;
                   (taints_acc, shape_acc, lval_env)
               | ToReturn
                   {
                     data_taints = taints;
                     data_shape = shape;
                     control_taints;
                     guards = inner_guards;
                     _;
                   } ->
                   (* Conjoin the callee's rebound guard onto each guarded
                    * taint. The guarded taints travel with the value through the
                    * outer's storage; at outer's later emission sites
                    * ([effects_of_tainted_return], [record_effects],
                    * [effects_from_arg_updates_at_exit]) one effect emerges
                    * per guarded taint, each carrying its own guard. Fan-in of
                    * disjoint inner branches is preserved as separate
                    * guarded taints in [Taint_set] and fused via [compose_or] at
                    * joins — the smart-constructor complement rule then
                    * folds [G or not G] to [top]. *)
                   let taints = Taints.conjoin_guard inner_guards taints in
                   (* One ToReturn per return statement: fold them as the
                    * join of the returned values, so a Clean field of one
                    * does not hide the whole taint of another. *)
                   let (S.Cell (xtaint, shape)) =
                     Shape.unify_cell ~lang:env.taint_inst.lang
                       (S.Cell (Xtaint.of_taints taints, shape))
                       (S.Cell (Xtaint.of_taints taints_acc, shape_acc))
                   in
                   ( Xtaint.to_taints xtaint,
                     shape,
                     Lval_env.add_control_taints lval_env control_taints )
               | ToLval { taints; var; offset; guards } ->
                   if not (is_own_param env var) then
                     record_effects env
                       [ Effect.ToLval
                           { taints;
                             lval = { base = base_of_free_var env var; offset };
                             guards } ];
                   (* The written taints carry the (possibly deferred) guard
                    * into the environment, so a later sink sees it as the
                    * item guard. *)
                   let taints = Taints.conjoin_guard guards taints in
                   ( taints_acc,
                     shape_acc,
                     lval_env
                     |> Lval_env.add_written_through env.taint_inst.lang var offset
                          taints )
               | ToLvalThis { taints; offset; guards } ->
                   record_this_field_write env taints offset guards;
                   (* Mirror the sibling [ToLval] arm's local write. *)
                   let taints = Taints.conjoin_guard guards taints in
                   ( taints_acc,
                     shape_acc,
                     add_this_field_to_lval_env env lval_env offset taints )
               | ToSinkInCall
                   {
                     callee;
                     arg;
                     arg_offset;
                     args_taints;
                     guards = rebound_guards;
                   } ->
                   resolve_preserved_to_sink_in_call env ~callee ~arg
                     ~arg_offset ~args_taints ~rebound_guards
                     (taints_acc, shape_acc, lval_env))
             (Taints.empty, Bot, env.lval_env))
  | None ->
      Log.debug (fun m ->
          m "CHECK_FUNCTION_CALL: No signature found for %s, returning None"
            (Display_IL.string_of_exp fun_exp));
      None

let check_function_call_callee ~(arity : int) env e =
  match e.e with
  | Fetch ({ base = _; rev_offset = _ :: _ } as lval) ->
      (* Method call <object ...>.<method>, the 'sub_taints' and 'sub_shape'
       * correspond to <object ...>. *)
      Log.debug (fun m ->
          m "METHOD_CALL_CALLEE: %s (lval: %s)"
            (Display_IL.string_of_exp e)
            (Display_IL.string_of_lval lval));
      let taints, shape, `Sub (sub_taints, sub_shape), lval_env =
        check_tainted_lval env lval
      in
      let obj_taints = sub_taints |> add_taints_from_shape sub_shape in
      Log.debug (fun m ->
          m "METHOD_CALL_CALLEE: obj_taints=%s, sub_taints=%s, returning taints=%s"
            (T.show_taints obj_taints)
            (T.show_taints sub_taints)
            (T.show_taints taints));
      (* Return sub_shape so we can check if the base object is a function parameter *)
      (`Obj (obj_taints, sub_shape), taints, shape, lval_env)
  | __else__ ->
      let taints, shape, lval_env = check_tainted_expr ~arity env e in
      (`Fun, taints, shape, lval_env)

(* Test whether an instruction is tainted, and if it is also a sink,
 * report the effect too (by side effect). *)
let call_with_intrafile lval_opt e env args instr =
  let args_taints, all_args_taints, lval_env =
    check_function_call_arguments env args
  in
  let all_args_taints =
    all_args_taints
    |> Taints.union (gather_all_taints_in_args_taints args_taints)
  in
  let arity = List.length args in
  let e_obj, e_taints, e_shape, lval_env =
    check_function_call_callee ~arity { env with lval_env } e
  in
  check_orig_if_sink { env with lval_env } instr.iorig all_args_taints Bot
    ~filter_sinks:(fun m -> not (m.spec.sink_exact && m.spec.sink_has_focus));
  let call_taints, shape, lval_env =
    (* Constructor call handling for ClassName() and ClassName.new():
       the callee bare name's [id_callee_definition] sids point at the resolved
       defs (stamped by extraction), and a construction resolves to the ctor
       def (e.g. [__init__]/[initialize]), so a sid whose bare name is a
       constructor's decides.
       A construction must not be mistaken for an implicit block/HOF call,
       and its callee is remapped below so Sig_inst maps BThis onto the
       assignment target. *)
    let resolves_to_constructor =
      (* Method calls on objects (e.g., _tmp.get_data()) should not be
         remapped as constructors. Their eorig may share a token with a
         constructor edge (e.g., in Passthrough(source()).get_data(), both
         the constructor and the method eorig start at "Passthrough").
         Skip the constructor check for Dot accesses unless it's Ruby's or
         Crystal's ClassName.new() pattern. *)
      (match e.e with
      | Fetch { rev_offset = [{ o = Dot name; _ }]; _ }
        when fst name.IL.ident <> "new"
             || not Lang.(env.taint_inst.lang =*= Ruby || env.taint_inst.lang =*= Crystal) -> false
      | _ -> true) &&
      Option.is_some env.signature_db &&
      let callee_definition_sids = match e.e with
        | Fetch { base = Var name; rev_offset = [] } ->
            !(name.id_info.G.id_callee_definition)
        | Fetch { base = Var _; rev_offset = [ { o = Dot m; _ } ] } ->
            !(m.id_info.G.id_callee_definition)
        | _ -> []
      in
      List.exists
        (fun (sid : G.SId.t) ->
          (not (G.SId.is_unsafe_default sid))
          &&
          let (rname, _, _, _) = G.SId.to_loc sid in
          Object_initialization.is_constructor env.taint_inst.lang rname None
          || Visit_function_defs.is_class_initialiser_ident rname)
        callee_definition_sids
    in
    (* Detect Ruby/Scala/Kotlin implicit block pattern:
     * When a call has a single lambda argument (as a Fetch of a lambda lval),
     * and the callee is a Call expression, treat it as calling the inner method
     * with the lambda as an implicit block *)
    let implicit_lambda_call =
      if resolves_to_constructor then None
      else
      (match args with
      | [ arg ] ->
          (match arg with
          | IL.Unnamed ({ e = Fetch lval; _ } as lambda_exp) ->
              (* Single Fetch argument - check if it's a lambda by looking at its shape *)
              (match
                 Lval_env.find_lval env.taint_inst.lang env.lval_env lval
               with
              | Some (S.Cell (_, shape)) ->
                  (match shape with
                  | S.Fun _ ->
                      (* It's a function/lambda! *)
                      Some (e, lambda_exp)
                  | _ -> None)
              | None -> None)
          | _ -> None)
      | _ -> None)
    in
    (* Handle implicit lambda pattern FIRST, before trying constructor *)
    match implicit_lambda_call with
    | Some (inner_e, lambda_exp) ->
        (* Trace back to find the original call expression that was assigned to inner_e.
         * For Ruby, inner_e is typically _tmp:N which was assigned from arr.map().
         * We need to use arr.map (not _tmp) for signature lookup. *)
        (* For Ruby implicit blocks, inner_e is typically Fetch(_tmp) where _tmp
         * has a Fun shape from calling arr.map(). We need to directly instantiate
         * that Fun shape instead of doing signature database lookup. *)
        (match inner_e.e with
        | Fetch lval ->
            (* Check the shape of this lval to see if it has a Fun signature *)
            (match
               Lval_env.find_lval env.taint_inst.lang env.lval_env lval
             with
            | Some (S.Cell (var_taints, S.Fun (c, cs))) ->
                (* The variable has a Fun shape. Instantiate it directly instead of
                 * doing signature database lookup. *)
                let lambda_arg = IL.Unnamed lambda_exp in
                (* Get the taints from the array (BThis) which were stored when arr.map() was called.
                 * These are in var_taints (the xtaint of _tmp). *)
                let callback_arg_taints = Xtaint.to_taints var_taints in
                (* Get the lambda's Fun shape from the lval_env *)
                let lambda_shape =
                  (match lambda_exp.e with
                  | Fetch lval ->
                      (match
                         Lval_env.find_lval env.taint_inst.lang env.lval_env
                           lval
                       with
                      | Some (S.Cell (_, shape)) -> shape
                      | None -> S.Bot)
                  | _ -> S.Bot)
                in
                let lambda_arg_taint = IL.Unnamed (callback_arg_taints, lambda_shape) in
                let args_taints = [lambda_arg_taint] in
                (* Callback lookup in both modes; hazard contained by [preserve_effect]. *)
                let call_effects =
                  c :: cs
                  |> List.concat_map (fun (closure : S.closure) ->
                         Sig_inst.instantiate_function_signature
                           ~lang:env.taint_inst.lang
                           ~atoms:env.shared_tables.guard_atoms
                           ~max_offset:(poly_offset_bound env inner_e)
                           ~outer_params:env.func.il_params ~env:closure.env
                           env.lval_env closure.sig_ ~callee:inner_e
                           ~args:(Some [ lambda_arg ]) args_taints
                           ~lookup_sig:(lookup_signature env) ())
                in
                    (* ToSinkInCall effects should have been recursively instantiated by Sig_inst,
                     * so we just need to process the resulting effects *)
                    (* Process the call effects to get taints and shape *)
                    let call_taints, shape, lval_env =
                      List.fold_left
                        (fun (taints_acc, shape_acc, lval_env) (call_effect : Sig_inst.call_effect) ->
                          match call_effect with
                          | ToSink { taints_with_precondition = incoming_taints, _; sink; _ } ->
                              let sink_effects = effects_of_tainted_sink env incoming_taints sink in
                              record_effects env sink_effects;
                              (taints_acc, shape_acc, lval_env)
                          | ToReturn { data_taints; data_shape; _ } ->
                              (Taints.union taints_acc data_taints,
                               Shape.unify_shape ~lang:env.taint_inst.lang
                                 data_shape shape_acc,
                               lval_env)
                          | ToLval { taints; var = lval_name; offset; _ } ->
                              let lval_env =
                                Lval_env.add env.taint_inst.lang lval_name
                                  offset taints lval_env
                              in
                              (taints_acc, shape_acc, lval_env)
                          | ToLvalThis { taints; offset; guards } ->
                              record_this_field_write env taints offset guards;
                              (* Mirror the sibling [ToLval] arm's local write
                                 (no guard conjoin here, as in the sibling). *)
                              ( taints_acc,
                                shape_acc,
                                add_this_field_to_lval_env env lval_env offset
                                  taints )
                          | ToSinkInCall
                              {
                                callee;
                                arg;
                                arg_offset;
                                args_taints;
                                guards = rebound_guards;
                              } ->
                              resolve_preserved_to_sink_in_call env ~callee
                                ~arg ~arg_offset ~args_taints ~rebound_guards
                                (taints_acc, shape_acc, lval_env))
                        (Taints.empty, S.Bot, env.lval_env)
                        call_effects
                    in
                    (call_taints, shape, lval_env)
            | Some (S.Cell (_, _)) ->
                (* Try signature lookup instead *)
                (match check_function_call { env with lval_env } inner_e args args_taints () with
                | Some (call_taints, shape, lval_env) ->
                    (call_taints, shape, lval_env)
                | None ->
                    (all_args_taints, S.Bot, lval_env))
            | None ->
                (* Try signature lookup instead *)
                (match check_function_call { env with lval_env } inner_e args args_taints () with
                | Some (call_taints, shape, lval_env) ->
                    (call_taints, shape, lval_env)
                | None ->
                    (all_args_taints, S.Bot, lval_env)))
        | _ ->
            (* Try signature lookup instead *)
            (match check_function_call { env with lval_env } inner_e args args_taints () with
            | Some (call_taints, shape, lval_env) ->
                (call_taints, shape, lval_env)
            | None ->
                (all_args_taints, S.Bot, lval_env)))
    | None ->
        (* When taint flows through a constructor (e.g., `obj = Foo(tainted)`),
         * the constructor signature may contain ToLval(BThis.field, taint)
         * effects that assign taint to fields of the new object.
         * Remap: ClassName() → obj.ClassName(), ClassName.new() → obj.ClassName()
         * This makes the callee a method-call shape so that Sig_inst maps
         * BThis to obj (the assignment target) when instantiating the
         * constructor's ToLval effects. *)
        let e =
          if resolves_to_constructor then
            match (lval_opt, e.e) with
            | Some lval, Fetch { base = Var name; rev_offset = [] } ->
                IL.{ e = Fetch { base = lval.base;
                                 rev_offset = [{ o = Dot name; oorig = NoOrig }] };
                     eorig = e.eorig }
            (* [ClassName.new()]: keep the [new] offset — it carries the
               ctor def's [id_callee_definition] stamp; the class-name base
               does not. *)
            | Some lval, Fetch { base = Var _; rev_offset = [ ({ o = Dot _; _ } as off) ] } ->
                IL.{ e = Fetch { base = lval.base; rev_offset = [ off ] };
                     eorig = e.eorig }
            | _ -> e
          else e
        in
        (* Receiver is stripped from sigs (reaches body as [BThis]); pass actuals verbatim — a synthetic [self] would shift every [BArg] index by one. *)
        (* No implicit lambda, try unified constructor execution *)
        let check_function_call_wrapper env' e' args' args_taints' =
          check_function_call env' e' args' args_taints' ()
        in
        match
          Object_initialization.execute_unified_constructor e args args_taints
            check_function_call_wrapper { env with lval_env }
        with
        | Some (call_taints, shape, lval_env) ->
            (* Constructor ToLval effects (e.g., this.data = tainted_arg)
             * update lval_env with field-level taint on the target variable,
             * but the return shape may still be Bot (constructors typically
             * don't return a value). Read back the shape from lval_env so
             * it propagates through intermediate assignments like
             * `_tmp = Foo(x); obj = _tmp`. Without this, the shape is lost
             * at the assignment boundary. *)
            let shape =
              if resolves_to_constructor then
                match lval_opt with
                | Some lval -> (
                    match
                      Lval_env.find_lval env.taint_inst.lang lval_env lval
                    with
                    | Some (S.Cell (_, s)) when
                      (match s with
                      | S.Bot -> false
                      | _ -> true) -> s
                    | _ -> shape)
                | None -> shape
              else shape
            in
            (call_taints, shape, lval_env)
        | None -> (
            match check_function_call { env with lval_env } e args args_taints () with
        | Some (call_taints, shape, lval_env) ->
            Log.debug (fun m ->
                m ~tags:sigs_tag "- Instantiating %s: returns %s & %s"
                  (Display_IL.string_of_exp e)
                  (T.show_taints call_taints)
                  (S.show_shape shape));
            (call_taints, shape, lval_env)
        | None -> (
            Log.debug (fun m ->
                m "INTRAFILE: No signature found for %s, falling back to propagation" (Display_IL.string_of_exp e));
            Log.debug (fun m ->
                m "INTRAFILE: all_args_taints = %s, propagate_through_functions = %b"
                  (T.show_taints all_args_taints)
                  (propagate_through_functions env));
            let call_taints =
              if not (propagate_through_functions env) then Taints.empty
              else
                (* Otherwise assume that the function will propagate
                 * the taint of its arguments. *)
                all_args_taints
            in
            Log.debug (fun m ->
                m "INTRAFILE: Returning call_taints = %s"
                  (T.show_taints call_taints));

            match
              propagate_taint_via_java_getters_and_setters_without_definition
                { env with lval_env } e args all_args_taints
            with
            | Some (getter_taints, _TODOshape, lval_env) ->
                (* HACK: Java: If we encounter `obj.setX(arg)` we interpret it as
                 * `obj.x = arg`, if we encounter `obj.getX()` we interpret it as
                 * `obj.x`. *)
                let call_taints = Taints.union call_taints getter_taints in
                (call_taints, Bot, lval_env)
            | None ->
                (* We have no taint signature and it's neither a get/set method. *)
                if not (propagate_through_functions env) then
                  (Taints.empty, Bot, lval_env)
                else (
                  (* Check if this is a call that invokes a callback parameter:
                   * - Direct call: f(x) where f is a callback (e_shape is S.Arg)
                   * - Method call: f.apply(x) or f.call(x) where f is a callback (e_obj is S.Arg)
                   * In this case we return empty taints - the callback's return will be handled
                   * when the ToSinkInCall effect is instantiated. *)
                  let is_method_callback_invoke =
                    (* Check if this is a method call on a callback parameter
                     * via a configured invoke method (e.g. .apply, .call, .run). *)
                    match e_obj, e.e with
                    | `Obj (_, S.Arg _), Fetch { rev_offset = { o = Dot name; _ } :: _; _ } ->
                        let invoke_methods = (Lang_config.get env.taint_inst.lang).invoke_methods in
                        List.mem (fst name.ident) invoke_methods
                    | _ -> false
                  in
                  Log.debug (fun m ->
                      m "INVOKE_CHECK: e=%s e_shape=%s e_obj_is_Arg=%b invoke=%b"
                        (Display_IL.string_of_exp e)
                        (S.show_shape e_shape)
                        (match e_obj with
                        | `Obj (_, S.Arg _) -> true
                        | _ -> false)
                        is_method_callback_invoke);
                  let callee_is_callback =
                    match e_shape with
                    | S.Arg _ -> true
                    | _ -> is_method_callback_invoke
                  in
                  (* Record ToSinkInCall effects for any callback arguments being passed. *)
                  (* [f.apply(x)] calls the receiver; [o.cb(x)] calls the
                   * function held in the field [cb] of the receiver. *)
                  let callee_shape =
                    match (e_obj, e.e) with
                    | `Obj (_, (S.Arg _ as shape)), _ when is_method_callback_invoke
                      ->
                        shape
                    | ( `Obj (_, (S.Arg _ as shape)),
                        Fetch { rev_offset = { o = Dot fld; _ } :: _; _ } ) -> (
                        match
                          Shape.find_in_shape_poly
                            ~max:(Shape.max_poly_offset env.taint_inst.lang)
                            ~lang:env.taint_inst.lang ~taints:Taints.empty
                            [ T.Ofld fld ] shape
                        with
                        | Some (_, field_shape) -> field_shape
                        | None -> e_shape)
                    | _ -> e_shape
                  in
                  effects_of_call_func_arg e callee_shape args_taints
                  |> record_effects { env with lval_env };
                  (* If the callee IS a callback parameter, its result is the
                   * result of that call, known when the signature is applied.
                   * This prevents false positives like sink(app(b, source())) where b doesn't
                   * propagate taint. But if we're just passing a callback TO another function,
                   * we still need to propagate taints normally. *)
                  if callee_is_callback then
                    let taints, shape =
                      result_of_call_func_arg ~lang:env.taint_inst.lang e
                        callee_shape
                    in
                    (taints, shape, lval_env)
                  else (
                    (* Callee is not a callback - propagate taints normally *)
                    let call_taints =
                      match e_obj with
                      | `Fun -> call_taints
                      | `Obj (obj_taints, _) -> call_taints |> Taints.union obj_taints
                    in
                    let result_taints, result_shape =
                      result_of_call_func_arg ~lang:env.taint_inst.lang e
                        callee_shape
                    in
                    (Taints.union result_taints call_taints, result_shape, lval_env)))))
  in
  (* We add the taint of the function itselt (i.e., 'e_taints') too. *)
  let all_call_taints =
    if env.taint_inst.options.taint_only_propagate_through_assignments then
      call_taints
    else Taints.union e_taints call_taints
  in
  let all_call_taints =
    check_type_and_drop_taints_if_bool_or_number env all_call_taints
      type_of_expr e
  in
  Log.debug (fun m ->
      m "CALL_RESULT: %s -> e_taints=%d call_taints=%d all=%d shape=%s"
        (Display_IL.string_of_exp e)
        (Taints.cardinal e_taints) (Taints.cardinal call_taints)
        (Taints.cardinal all_call_taints) (S.show_shape shape));
  (* Handle result variable assignment for Call instruction *)
  let lval_env =
    match lval_opt with
    | Some result_lval ->
        Lval_env.add_lval env.taint_inst.lang result_lval all_call_taints
          lval_env
    | None -> lval_env
  in
  (all_call_taints, shape, lval_env)

(* An object built without a known constructor carries the taints of all its
 * arguments, and each named argument is the field of that name. *)
let new_without_signature env args_taints all_args_taints lval_env =
  let all_args_taints =
    if env.taint_inst.options.taint_only_propagate_through_assignments then
      Taints.empty
    else
      all_args_taints
      |> Taints.union (gather_all_taints_in_args_taints args_taints)
  in
  let shape =
    match
      args_taints
      |> List.filter_map (function
           | IL.Named (ident, (taints, shape)) ->
               let field : IL.name =
                 { ident; sid = G.SId.unsafe_default; id_info = G.empty_id_info () }
               in
               Some (`Field (field, taints, shape))
           | IL.Unnamed _ -> None)
    with
    | [] -> S.Bot
    | fields -> Shape.record_or_dict_like_obj ~lang:env.taint_inst.lang fields
  in
  (all_args_taints, shape, lval_env)

let new_with_intrafile env _result_lval _ty args constructor =
  (* 'New' with reference to constructor - use constructor signatures *)
  let args_taints, all_args_taints, lval_env =
    check_function_call_arguments env args
  in
  let call_result =
    (* Try unified constructor execution first *)
    let check_function_call_wrapper env' e' args' args_taints' =
      check_function_call env' e' args' args_taints' ()
    in
    match
      Object_initialization.execute_unified_constructor constructor args
        args_taints check_function_call_wrapper { env with lval_env }
    with
    | Some (call_taints, shape, lval_env) -> Some (call_taints, shape, lval_env)
    | None ->
        check_function_call { env with lval_env } constructor args args_taints ()
  in
  match call_result with
  | Some (call_taints, shape, lval_env) -> (call_taints, shape, lval_env)
  | None -> new_without_signature env args_taints all_args_taints lval_env

let check_tainted_instr env instr : Taints.t * S.shape * Lval_env.t =
  let check_expr env = check_tainted_expr env in
  let check_instr = function
    | Assign (lval, e) ->
        let taints, shape, lval_env = check_expr env e in
        let taints =
          check_type_and_drop_taints_if_bool_or_number env taints type_of_expr e
        in
        (* Generate ToLval effect for instance variable assignments when intrafile is enabled *)
        (if env.taint_inst.options.taint_intrafile then
           match lval.base with
           | VarSpecial (Self, _)
           | VarSpecial (This, _)
             when not (Taints.is_empty taints) ->
               let offset =
                 T.offset_of_rev_IL_offset env.taint_inst.lang
                   ~rev_offset:lval.rev_offset
               in
               let taint_lval = { T.base = T.BThis; offset } in
               let effects =
                 [
                   Effect.ToLval
                     {
                       taints;
                       lval = taint_lval;
                       guards = Effect_guard.top;
                     };
                 ]
               in
               record_effects env effects
           | _ -> ());
        (* Let the transfer function handle the actual lval assignment *)
        (taints, shape, lval_env)
    | AssignAnon (lval, anon_entity) -> (
        match anon_entity with
        | Lambda _ -> (
            (* For lambdas, look up their signature from the signature database *)
            match (lval.base, env.signature_db, anon_entity) with
            | Var lambda_name, Some db, Lambda fdef ->
                let arity = List.length fdef.fparams in
                (match Shape_and_sig.lookup_definition db (Function_id.of_il_name lambda_name) arity with
                | Some ((_, sig_) as found) ->
                    let fun_shape =
                      Shape_and_sig.closure_of_definition found
                        (closure_env env sig_)
                    in
                    Log.debug (fun m ->
                        m "AssignAnon: lambda %s has signature shape %s"
                          (IL.str_of_name lambda_name)
                          (S.show_shape fun_shape));
                    (Taints.empty, fun_shape, env.lval_env)
                | None ->
                    Log.debug (fun m ->
                        m "AssignAnon: lambda %s has no signature in db"
                          (IL.str_of_name lambda_name));
                    (Taints.empty, Bot, env.lval_env))
            | _, _, _ -> (Taints.empty, Bot, env.lval_env))
        | AnonClass _cdef ->
            (* Anonymous class instantiations are detected by Object_initialization.ml
             * before dataflow analysis and added to object_mappings. *)
            (Taints.empty, Bot, env.lval_env))
    | Call (lval_opt, e, args) ->
        let intrafile = env.taint_inst.options.taint_intrafile in
        if intrafile then call_with_intrafile lval_opt e env args instr
        else
          let args_taints, all_args_taints, lval_env =
            check_function_call_arguments env args
          in
          let all_args_taints =
            all_args_taints
            |> Taints.union (gather_all_taints_in_args_taints args_taints)
          in
          let arity = List.length args in
          let e_obj, e_taints, e_shape, lval_env =
            check_function_call_callee ~arity { env with lval_env } e
          in
          (* NOTE(sink_has_focus):
           * After we made sink specs "exact" by default, we need this trick to
           * be backwards compatible wrt to specifications like `sink(...)`. Even
           * if the sink is "exact", if it has NO focus, then we consider that all
           * of the parameters of the function are sinks. So, even if
           * `taint_assume_safe_functions: true`, if the spec is `sink(...)`, we
           * still report `sink(tainted)`.
           *)
          check_orig_if_sink { env with lval_env } instr.iorig all_args_taints
            Bot ~filter_sinks:(fun m ->
              not (m.spec.sink_exact && m.spec.sink_has_focus));
          let call_taints, shape, lval_env =
            match
              check_function_call { env with lval_env } e args args_taints ()
            with
            | Some (call_taints, shape, lval_env) ->
                (* THINK: For debugging, we could print a diff of the previous and new lval_env *)
                Log.debug (fun m ->
                    m ~tags:sigs_tag "- Instantiating %s: returns %s & %s"
                      (Display_IL.string_of_exp e)
                      (T.show_taints call_taints)
                      (S.show_shape shape));
                (call_taints, shape, lval_env)
            | None -> (
                let call_taints =
                  if not (propagate_through_functions env) then Taints.empty
                  else
                    (* Otherwise assume that the function will propagate
                     * the taint of its arguments. *)
                    all_args_taints
                in
                match
                  propagate_taint_via_java_getters_and_setters_without_definition
                    { env with lval_env } e args all_args_taints
                with
                | Some (getter_taints, _TODOshape, lval_env) ->
                    (* HACK: Java: If we encounter `obj.setX(arg)` we interpret it as
                     * `obj.x = arg`, if we encounter `obj.getX()` we interpret it as
                     * `obj.x`. *)
                    let call_taints = Taints.union call_taints getter_taints in
                    (call_taints, Bot, lval_env)
                | None ->
                    (* We have no taint signature and it's neither a get/set method. *)
                    if not (propagate_through_functions env) then
                      (Taints.empty, Bot, lval_env)
                    else (
                      (* Check if this is a call to a function parameter (either direct or via method) *)
                      (match e_obj with
                      | `Obj (_obj_taints, S.Arg _) ->
                          (* This is a method call on a function parameter (e.g., callback.apply in Java,
                           * callback.call in Ruby). Treat it as invoking the callback. *)
                          effects_of_call_func_arg e (match e_obj with `Obj (_, shape) -> shape | `Fun -> e_shape) args_taints
                          |> record_effects { env with lval_env }
                      | _ ->
                          effects_of_call_func_arg e e_shape args_taints
                          |> record_effects { env with lval_env });
                      (* If this is a method call, `o.method(...)`, then we fetch the
                       * taint of the callee object `o`. This is a conservative worst-case
                       * asumption that any taint in `o` can be tainting the call's effect. *)
                      let call_taints =
                        match e_obj with
                        | `Fun -> call_taints
                        | `Obj (obj_taints, _) ->
                            call_taints |> Taints.union obj_taints
                      in
                      (call_taints, Bot, lval_env)))
          in
          (* We add the taint of the function itselt (i.e., 'e_taints') too. *)
          let all_call_taints =
            if env.taint_inst.options.taint_only_propagate_through_assignments
            then call_taints
            else Taints.union e_taints call_taints
          in
          let all_call_taints =
            check_type_and_drop_taints_if_bool_or_number env all_call_taints
              type_of_expr e
          in
          (all_call_taints, shape, lval_env)
    | New (result_lval, ty, Some constructor, args) -> (
        if env.taint_inst.options.taint_intrafile then
          new_with_intrafile env result_lval ty args constructor
        else
          let args_taints, all_args_taints, lval_env =
            check_function_call_arguments env args
          in
          match
            check_function_call { env with lval_env } constructor args
              args_taints ()
          with
          | Some (call_taints, shape, lval_env) -> (call_taints, shape, lval_env)
          | None ->
              new_without_signature env args_taints all_args_taints lval_env)
    | New (_lval, _ty, None, args) ->
        (* 'New' without reference to constructor *)
        let args_taints, all_args_taints, lval_env =
          check_function_call_arguments env args
        in
        new_without_signature env args_taints all_args_taints lval_env
    | CallSpecial (_, (op, _), args) ->
        let args_taints, all_args_taints, lval_env =
          check_function_call_arguments env args
        in
        let all_args_taints =
          all_args_taints
          |> Taints.union (gather_all_taints_in_args_taints args_taints)
        in
        let all_args_taints =
          if env.taint_inst.options.taint_only_propagate_through_assignments
          then Taints.empty
          else all_args_taints
        in
        (* For C function pointers (&func), look up the function signature. *)
        let shape =
          match (op, args) with
          | IL.Ref, [ IL.Unnamed exp ] -> (
              match (lookup_signature env exp 0, args_taints) with
              | (_ :: _ as found), _ -> closure_set_of_definitions env found
              | [], [ IL.Unnamed (_, arg_shape) ] -> arg_shape
              | [], _ -> Bot)
          | _ -> Bot
        in
        (all_args_taints, shape, lval_env)
    | FixmeInstr _ -> (Taints.empty, Bot, env.lval_env)
  in
  let sanitizer_pms = orig_is_best_sanitizer env instr.iorig in
  match sanitizer_pms with
  (* See NOTE [is_sanitizer] *)
  | _ :: _ ->
      (* TODO: We should check that taint and sanitizer(s) are unifiable. *)
      (Taints.empty, Bot, env.lval_env)
  | [] ->
      let taints_instr, rhs_shape, lval_env = check_instr instr.i in
      let taint_sources, lval_env =
        orig_is_best_source env instr.iorig
        |> taints_of_matches { env with lval_env } ~incoming:taints_instr
      in
      let taints = Taints.union taints_instr taint_sources in
      let taints_propagated, lval_env =
        handle_taint_propagators { env with lval_env } (`Ins instr) taints
          rhs_shape
      in
      let taints = Taints.union taints taints_propagated in
      check_orig_if_sink env instr.iorig taints rhs_shape;
      let taints =
        match LV.lval_of_instr_opt instr with
        | None -> taints
        | Some lval ->
            check_type_and_drop_taints_if_bool_or_number env taints type_of_lval
              lval
      in
      (taints, rhs_shape, lval_env)
[@@profiling]

(* Test whether a `return' is tainted, and if it is also a sink,
 * report the effect too (by side effect). *)
let check_tainted_return env tok e : Taints.t * S.shape * Lval_env.t =
  let sinks =
    any_is_best_sink env (G.Tk tok) @ orig_is_best_sink env e.eorig
    |> List.filter (TM.is_best_match env.func.best_matches)
    |> List_.map TM.sink_of_match
  in
  let taints, shape, var_env' = check_tainted_expr env e in
  let taints =
    (* TODO: Clean shape as well based on type ? *)
    check_type_and_drop_taints_if_bool_or_number env taints type_of_expr e
  in
  let effects = effects_of_tainted_sinks env taints sinks in
  record_effects env effects;
  (taints, shape, var_env')

let vars_shared_with_closures (effects : Effects.t) : IL.NameSet.t =
  let rec of_shape acc (shape : S.shape) =
    match shape with
    | Bot
    | Arg _ ->
        acc
    | Obj obj ->
        Shape_and_sig.Fields.fold
          (fun _ (S.Cell (_, shape)) acc -> of_shape acc shape)
          obj acc
    | Fun (c, cs) ->
        List.fold_left
          (fun acc (closure : S.closure) ->
            List.fold_left
              (fun acc (_, entry) ->
                match entry with
                | S.Ref { base = T.BGlob v; _ } -> IL.NameSet.add v acc
                | S.Ref _ -> acc
                | S.Val (S.Cell (_, shape)) -> of_shape acc shape)
              acc closure.env)
          acc (c :: cs)
  in
  Effects.fold
    (fun eff acc ->
      match eff with
      | Effect.ToReturn { data_shape; _ } -> of_shape acc data_shape
      | Effect.ToSinkInCall { args_taints; _ } ->
          List.fold_left
            (fun acc -> function
              | IL.Unnamed (_, shape)
              | IL.Named (_, (_, shape)) ->
                  of_shape acc shape)
            acc args_taints
      | Effect.ToSink _
      | Effect.ToLval _ ->
          acc)
    effects IL.NameSet.empty

(* A write to a variable declared inside the function cannot be observed
 * outside it; a lambda's writes to variables it captures stay, and so do
 * writes to a local that a closure leaving the function refers to. *)
let drop_writes_to_own_vars (fun_cfg : IL.fun_cfg) (effects : Effects.t) :
    Effects.t =
  match fun_cfg.source_range with
  | None -> effects
  | Some range ->
      let shared = vars_shared_with_closures effects in
      effects
      |> Effects.filter (function
           | Effect.ToLval { lval = { base = T.BGlob var; _ }; _ } ->
               (not (IL_helpers.declared_in_range range var))
               || IL.NameSet.mem var shared
           | _ -> true)

let captured_vars (fun_cfg : IL.fun_cfg) : IL.NameSet.t =
  match fun_cfg.source_range with
  | None -> IL.NameSet.empty
  | Some range ->
      let cfgs =
        fun_cfg :: List_.map snd (collect_all_lambdas_innermost_first fun_cfg)
      in
      let own_params =
        cfgs
        |> List.concat_map (fun (cfg : IL.fun_cfg) ->
               List.filter_map IL_helpers.pname_of_param cfg.params)
        |> IL.NameSet.of_list
      in
      let is_captured (name : IL.name) =
        match !(name.id_info.id_resolved) with
        | Some ((G.LocalVar | G.Parameter), _) ->
            (not (IL.NameSet.mem name own_params))
            && not (IL_helpers.declared_in_range range name)
        | _ -> false
      in
      cfgs
      |> List.fold_left
           (fun acc (cfg : IL.fun_cfg) ->
             LV.reachable_nodes cfg
             |> Seq.fold_left
                  (fun acc (node : IL.node) ->
                    LV.rlvals_of_node node.n
                    |> List.fold_left
                         (fun acc (lval : IL.lval) ->
                           match lval.base with
                           | Var name when is_captured name ->
                               IL.NameSet.add name acc
                           | _ -> acc)
                         acc)
                  acc)
           IL.NameSet.empty

let captured_of_fun_cfg (fun_cfg : IL.fun_cfg) :
    (IL.name * AST_generic.capture_mode) list =
  let listed = fun_cfg.captures.clist in
  let implicit_mode =
    Option.value fun_cfg.captures.cdefault ~default:G.Capture_by_reference
  in
  listed
  @ (captured_vars fun_cfg |> IL.NameSet.elements
    |> List.filter (fun name ->
           not
             (List.exists
                (fun (x, _) -> Int.equal (IL.compare_name x name) 0)
                listed))
    |> List_.map (fun name -> (name, implicit_mode)))

(* While a signature is built, a captured variable stands for the value the
 * closure's environment gives it when the signature is applied. *)
let seed_captured_vars (lang : Lang.t)
    (captured : (IL.name * AST_generic.capture_mode) list) (env : Lval_env.t) :
    Lval_env.t =
  match captured with
  | [] -> env
  | _ ->
      let is_captured (name : IL.name) =
        List.exists
          (fun (x, _) -> Int.equal (IL.compare_name x name) 0)
          captured
      in
      List.fold_left
        (fun env ((name : IL.name), _) ->
          Lval_env.add_lval_shape lang
            { base = Var name; rev_offset = [] }
            (Taints.singleton
               (T.taint_of_orig (T.Var { base = T.BEnv name; offset = [] })))
            (S.Arg (T.Captured name, [ [] ]))
            env)
        (Lval_env.filter_tainted (fun name -> not (is_captured name)) env)
        captured

(* The globals whose value at a call may differ from their initial one. A
 * read is left out when constant propagation proved the global holds one
 * scalar, and when the global is only the callee of a call: the function value's
 * own taint says nothing about the call's result. *)
let global_vars (fun_cfg : IL.fun_cfg) : IL.NameSet.t =
  let add_if_global acc (name : IL.name) =
    match !(name.id_info.id_resolved) with
    | Some (G.Global, _) -> IL.NameSet.add name acc
    | _ -> acc
  in
  let may_vary (name : IL.name) =
    match !(name.id_info.id_svalue) with
    | Some (G.Lit _ | G.Cst _) -> false
    | Some (G.Sym _ | G.NotCst)
    | None ->
        true
  in
  LV.reachable_nodes fun_cfg
  |> Seq.fold_left
       (fun acc (node : IL.node) ->
         let written, callee =
           match node.n with
           | NInstr ({ i = Call (_, { e = Fetch callee; _ }, _); _ } as instr)
             ->
               (Option.to_list (LV.lvar_of_instr_opt instr), Some callee)
           | NInstr instr -> (Option.to_list (LV.lvar_of_instr_opt instr), None)
           | _ -> ([], None)
         in
         let is_callee (lval : IL.lval) =
           match callee with
           | Some ({ base = Var _; rev_offset = [] } as callee) ->
               phys_equal lval callee
           | Some _
           | None ->
               false
         in
         LV.rlvals_of_node node.n
         |> List.filter_map (fun (lval : IL.lval) ->
                match lval.base with
                | Var name when (not (is_callee lval)) && may_vary name ->
                    Some name
                | _ -> None)
         |> List.rev_append written
         |> List.fold_left add_if_global acc)
       IL.NameSet.empty

(* While a signature is built, a global stands for the value it has when
 * the signature is applied. *)
let seed_global_vars (lang : Lang.t) (globals : IL.NameSet.t)
    (env : Lval_env.t) : Lval_env.t =
  IL.NameSet.fold
    (fun (name : IL.name) env ->
      Lval_env.add_lval lang
        { base = Var name; rev_offset = [] }
        (Taints.singleton
           (T.taint_of_orig (T.Var { base = T.BGlob name; offset = [] })))
        env)
    globals env

let rebound_vars (cfg : IL.cfg) : IL.NameSet.t =
  CFG.NodeiSet.fold
    (fun ni acc ->
      match (cfg.graph#nodes#assoc ni).IL.n with
      | NInstr instr -> (
          match LV.lval_of_instr_opt instr with
          | Some { base = Var name; rev_offset = [] } -> IL.NameSet.add name acc
          | _ -> acc)
      | _ -> acc)
    cfg.reachable IL.NameSet.empty

(* The parameter holds a copy of the caller's value (a struct passed by
 * value), so nothing the callee does to it reaches the caller. *)
let param_is_copy ~(is_value_type : G.type_ -> bool) (p : IL.name_param) :
    bool =
  (not p.by_reference)
  &&
  match p.ptype with
  | None -> false
  | Some { t = G.OtherType ((("struct" | "union" | "class"), _), _); _ } ->
      true
  | Some t -> is_value_type t

let copied_params ~(is_value_type : G.type_ -> bool)
    (params : IL.param list) : IL.NameSet.t =
  params
  |> List.fold_left
       (fun acc (p : IL.param) ->
         match p with
         | IL.Param np
         | IL.ParamKwd np
         | IL.ParamReceiver np
         | IL.ParamPattern (np, _)
           when param_is_copy ~is_value_type np ->
             IL.NameSet.add np.pname acc
         | _ -> acc)
       IL.NameSet.empty

(* A change to a parameter as a whole reaches the caller unless the function
 * rebinds the parameter, which the caller does not see for a parameter
 * passed by value; in-place changes and field writes reach it. Nothing
 * reaches the caller from a parameter or receiver that holds a copy. *)
let caller_sees_update (params : IL.param list) (rebound : IL.NameSet.t)
    (copied : IL.NameSet.t) (lval : T.lval) : bool =
  let own_param_with_name (name : string) =
    List.find_opt
      (fun (p : IL.param) ->
        match IL_helpers.pname_of_param p with
        | Some pname -> String.equal (fst pname.ident) name
        | None -> false)
      params
  in
  let copied_param (p : IL.param) =
    match IL_helpers.pname_of_param p with
    | Some pname -> IL.NameSet.mem pname copied
    | None -> false
  in
  match (lval.base, lval.offset) with
  | T.BArg arg, _ when Option.fold ~none:false ~some:copied_param
                         (own_param_with_name arg.name) ->
      false
  | T.BThis, _
    when List.exists
           (function
             | IL.ParamReceiver np -> IL.NameSet.mem np.pname copied
             | _ -> false)
           params ->
      false
  | T.BArg arg, [] -> (
      let own_param =
        List.find_opt
          (fun (p : IL.param) ->
            match IL_helpers.pname_of_param p with
            | Some pname -> String.equal (fst pname.ident) arg.name
            | None -> false)
          params
      in
      match own_param with
      | Some (IL.Param { pname; by_reference; _ }) ->
          by_reference || not (IL.NameSet.mem pname rebound)
      | Some p -> (
          match IL_helpers.pname_of_param p with
          | Some pname -> not (IL.NameSet.mem pname rebound)
          | None -> true)
      | None -> true)
  | _ -> true

(* The writes to the caller's objects that [var]'s value in [exit_var_ref]
 * shows, compared with [var] on entry. *)
let arg_updates_of_var ~(lang : Lang.t) ~(keep : T.lval -> bool) enter_env
    (var : IL.name) exit_var_ref : Effect.t Seq.t =
  match Lval_env.find_var enter_env var with
  | None -> Seq.empty
  | Some (Cell ((`Clean | `None), _)) -> Seq.empty
  | Some (Cell (`Tainted enter_taints, _) as enter_cell) -> (
      (* For each lval in the enter_env, we get its `T.lval`, and check
       * if it got new taints at the exit_env. If so, we generate a 'ToLval'. *)
      match
        enter_taints |> Taints.to_taint_list
        |> List_.filter_map (fun (taint : T.taint) ->
               match taint.T.orig with
               | T.Var lval -> Some lval
               | _ -> None)
      with
      | []
      | _ :: _ :: _ ->
          Seq.empty
      | [ lval ] ->
          Shape.enum_in_cell exit_var_ref
          |> Seq.filter_map (fun (offset, exit_taints) ->
                 let lval =
                   { lval with offset = lval.offset @ offset }
                 in
                 if not (keep lval) then None
                 else
                 let enter_taints_at_offset =
                   match Shape.find_in_cell ~lang offset enter_cell with
                   | `Found (Cell (xtaint, _)) -> Xtaint.to_taints xtaint
                   | `Not_found (carried, _, _) -> carried
                   | `Clean -> Taints.empty
                 in
                 let new_taints =
                   Taints.diff exit_taints enter_taints_at_offset
                 in
                 (* TODO: Also report if taints are _cleaned_. *)
                 if not (Taints.is_empty new_taints) then
                   Some
                     (Effect.ToLval
                        {
                          taints = new_taints;
                          lval;
                          (* The write may have happened under a branch
                           * guard; recover it from the guards the
                           * written taints carry (tagged at the write
                           * site) so a caller can drop the effect when
                           * its argument makes the guard false. *)
                          guards = Taints.guards_disjunction new_taints;
                        })
                 else None))

let effects_from_arg_updates_at_exit ~(lang : Lang.t) ~(params : IL.param list)
    ~(rebound : IL.NameSet.t) ~(copied : IL.NameSet.t) enter_env exit_env :
    Effect.t list =
  (* TOOD: We need to get a map of `lval` to `Taint.arg`, and if an extension
   * of `lval` has new taints, then we can compute its correspoding `Taint.arg`
   * extension and generate a `ToLval` effect too. *)
  exit_env |> Lval_env.seq_of_tainted
  |> Seq.map (fun (var, exit_var_ref) ->
         arg_updates_of_var ~lang
           ~keep:(caller_sees_update params rebound copied)
           enter_env var exit_var_ref)
  |> Seq.concat |> List.of_seq

(* Before a by-value parameter is rebound it still refers to the caller's
 * object, so its changes so far reach the caller. *)
let rec shape_has_closure_env (shape : S.shape) : bool =
  match shape with
  | Bot
  | Arg _ ->
      false
  | Obj obj ->
      Shape_and_sig.Fields.exists
        (fun _ (S.Cell (_, shape)) -> shape_has_closure_env shape)
        obj
  | Fun (c, cs) ->
      List.exists
        (fun (closure : S.closure) -> not (List_.null closure.env))
        (c :: cs)

let effect_has_closure_env (eff : Effect.t) : bool =
  match eff with
  | Effect.ToReturn { data_shape; _ } -> shape_has_closure_env data_shape
  | Effect.ToSinkInCall { args_taints; _ } ->
      List.exists
        (function
          | IL.Unnamed (_, shape)
          | IL.Named (_, (_, shape)) ->
              shape_has_closure_env shape)
        args_taints
  | Effect.ToSink _
  | Effect.ToLval _ ->
      false

(* A closure that leaves the function refers to the function's variables
 * through its environment. A parameter becomes the parameter as the
 * caller passed it; a variable the function captures stays in the
 * function's own closure environment; a local, or a parameter that holds
 * a copy or is rebound, lives on as a variable shared by the closures
 * that captured it, with its value at the exit. *)
let convert_escaping_closures ~(fun_cfg : IL.fun_cfg)
    ~(captured : (IL.name * AST_generic.capture_mode) list) ~(rebound : IL.NameSet.t)
    ~(copied : IL.NameSet.t) (exit_env : Lval_env.t) (effects : Effects.t) :
    Effects.t =
  let param_args =
    let explicit =
      List.filter
        (function
          | IL.ParamReceiver _ -> false
          | _ -> true)
        fun_cfg.params
    in
    List.combine explicit (Signature_params.of_IL_params explicit)
    |> List.mapi (fun index (p, sig_param) ->
           match (IL_helpers.pname_of_param p, sig_param) with
           | ( Some pname,
               (Signature_params.P name | Signature_params.PRest name) )
             when not (IL.NameSet.mem pname rebound || IL.NameSet.mem pname copied)
             ->
               Some (pname, { T.name; index })
           | _ -> None)
    |> List.filter_map Fun.id
  in
  let same (x : IL.name) (y : IL.name) = Int.equal (IL.compare_name x y) 0 in
  let is_own_var (v : IL.name) =
    List.exists
      (fun p ->
        match IL_helpers.pname_of_param p with
        | Some pname -> same pname v
        | None -> false)
      fun_cfg.params
    ||
    match fun_cfg.source_range with
    | Some range -> IL_helpers.declared_in_range range v
    | None -> false
  in
  let escaped = ref IL.NameSet.empty in
  let convert_ref (lval : T.lval) : T.lval =
    match lval.base with
    | BGlob v -> (
        match List.find_opt (fun (pname, _) -> same pname v) param_args with
        | Some (_, arg) -> { lval with base = BArg arg }
        | None ->
            if List.exists (fun (x, _) -> same x v) captured then
              { lval with base = BEnv v }
            else (
              if is_own_var v then escaped := IL.NameSet.add v !escaped;
              lval))
    | BArg _
    | BThis
    | BEnv _
    | BCall _ ->
        lval
  in
  let rec convert_shape (shape : S.shape) : S.shape =
    match shape with
    | Bot
    | Arg _ ->
        shape
    | Obj obj -> Obj (Shape_and_sig.Fields.map convert_cell obj)
    | Fun (c, cs) ->
        let c, cs =
          Shape_and_sig.map_closures
            (fun (closure : S.closure) ->
              {
                closure with
                env =
                  List_.map
                    (fun (x, entry) ->
                      match entry with
                      | S.Ref lval -> (x, S.Ref (convert_ref lval))
                      | S.Val cell -> (x, S.Val (convert_cell cell)))
                    closure.env;
              })
            (c, cs)
        in
        Fun (c, cs)
  and convert_cell (Cell (xtaint, shape)) = Cell (xtaint, convert_shape shape) in
  let convert_arg = function
    | IL.Unnamed (taints, shape) -> IL.Unnamed (taints, convert_shape shape)
    | IL.Named (id, (taints, shape)) -> IL.Named (id, (taints, convert_shape shape))
  in
  let effects =
    effects
    |> Effects.map (function
         | Effect.ToReturn ret ->
             Effect.ToReturn { ret with data_shape = convert_shape ret.data_shape }
         | Effect.ToSinkInCall call ->
             Effect.ToSinkInCall
               { call with args_taints = List_.map convert_arg call.args_taints }
         | (Effect.ToSink _ | Effect.ToLval _) as eff -> eff)
  in
  let escaped_values =
    IL.NameSet.elements !escaped
    |> List.concat_map (fun (v : IL.name) ->
           match Lval_env.find_var exit_env v with
           | None -> []
           | Some cell ->
               Shape.enum_in_cell cell |> List.of_seq
               |> List.filter_map (fun (offset, taints) ->
                      if Taints.is_empty taints then None
                      else
                        Some
                          (Effect.ToLval
                             {
                               taints;
                               lval = { base = BGlob v; offset };
                               guards = Effect_guard.top;
                             })))
  in
  Effects.add_list escaped_values effects

let effects_before_param_rebinding ~(lang : Lang.t) enter_env current_env
    (var : IL.name) : Effect.t list =
  match Lval_env.find_var current_env var with
  | None -> []
  | Some var_ref ->
      arg_updates_of_var ~lang ~keep:(fun _ -> true) enter_env var var_ref
      |> List.of_seq

let check_tainted_control_at_exit node env =
  match node.F.n with
  (* This is only for implicit returns, we could handle 'NReturn' here too
   * but we would be generating duplicate effects. *)
  | NReturn _ -> ()
  | __else__ ->
      if node.IL.at_exit then
        let return_tok =
          (* Getting a token from an arbitrary node could be expensive
           * (see 'AST_generic_helpers.range_of_tokens'). We just use a
           * fake one but use the function's name if available to make
           * it unique. If it were not unique, the effects cache in
           * 'Deep_tainting' would consider all `ToReturn`s with the
           * same control taint as being the same, given that
           * `Taint.compare_source` does not compare the length of the
           * call trace. And that could cause some calls to be missing
           * in the call trace of a finding. *)
          match env.func.name with
          | None -> G.fake "return"
          | Some name -> G.fake (IL.str_of_name name ^ "/return")
        in
        let effects =
          effects_of_tainted_return env ~several_results:false Taints.empty Bot
            return_tok
        in
        record_effects env effects

(*****************************************************************************)
(* Transfer *)
(*****************************************************************************)

let input_env ~lang ~enter_env ~(flow : F.cfg) mapping ni =
  let node = flow.graph#nodes#assoc ni in
  match node.F.n with
  | Enter -> enter_env
  | _else -> (
      let pred_envs =
        CFG.predecessors flow ni
        |> List_.map (fun (pi, _) -> mapping.(pi).D.out_env)
      in
      match pred_envs with
      | [] -> Lval_env.empty
      | [ penv ] -> penv
      | penv1 :: penvs -> List.fold_left (Lval_env.union ~lang) penv1 penvs)

(* Walk a [ParamPattern]'s inner pattern and enumerate each leaf
 * together with its offset path from the enclosing implicit binder.
 *
 * The offset path is the projection needed to reach the leaf from the
 * caller's actual argument. For example, the pattern [{body}] yields
 * one leaf [body] at offset [[Ofld "body"]]; [(a, b)] yields [a] at
 * offset [[Oint 0]] and [b] at offset [[Oint 1]]; [PatAs(inner, id)]
 * yields [id] at the same offset as [inner]'s root plus [inner]'s
 * own leaves.
 *
 * Map/dict destructures (Clojure [{:keys [body]}] / [{body :body}],
 * Elixir [%{body: body}]) arrive as a [PatList]/[PatConstructor] of
 * [PatKeyVal(lookup_key, binding)] — possibly wrapped in a single
 * [OtherPat("MapPairKeyword" | "MapPairArrow", …)] for Elixir's
 * [:]/[=>] source-level distinction. In that case the offset is the
 * key name (after stripping Clojure's [:] prefix and Elixir's [: ]
 * suffix), not the positional index.
 *
 * Pattern shapes without a clean structural offset path (PatDisj,
 * [PatKeyVal] with an opaque key, PatConstructor, PatWildcard,
 * PatLiteral, other [OtherPat] tags, PatWhen, …) contribute no
 * leaves — rule-focused source pre-seeding still runs on those via
 * [mk_fun_input_env] / [check_tainted_var], but they do not receive
 * an [Arg _] shape here. *)
let pattern_leaves_with_offsets ~(lang : Lang.t) (pat : AST_generic.pattern) :
    (IL.name * Taint.offset list) list =
  let mk_field (s, tok) : IL.name =
    {
      ident = (s, tok);
      sid = G.SId.unsafe_default;
      id_info = G.empty_id_info ();
    }
  in
  let stripped (s, tok) = (String_.strip_wrapping_char ':' s, tok) in
  (* The [OtherPat((":"|"::"), …)] branches below match the Clojure
   * parser's atom-key encoding; [:body] and [::body] denote distinct
   * runtime atoms and must produce distinct offsets. [G.Atom] cannot
   * currently carry the [:]/[::] distinction, so we reconstruct the
   * surface form from the tag prefix. Mirrors the handling in
   * [AST_to_IL.key_offset_of_pattern]; if [G.Atom] is later extended
   * with a kind, this branch collapses into the [PatLiteral (G.Atom
   * …)] branch. *)
  let ofld_of_key_pat (key_pat : G.pattern) : Taint.offset option =
    match key_pat with
    | G.PatId (id, _) -> Some (Taint.Ofld (mk_field (stripped id)))
    | G.PatLiteral (G.String (_, id, _)) ->
        Some (Taint.Ofld (mk_field (stripped id)))
    | G.PatLiteral (G.Atom (_, id)) ->
        Some (Taint.Ofld (mk_field (stripped id)))
    | G.OtherPat ((((":" | "::") as prefix), _), [ G.Name (G.Id (id, _)) ]) ->
        let s, tok = id in
        Some
          (Taint.Ofld
             (mk_field (prefix ^ String_.strip_wrapping_char ':' s, tok)))
    | G.OtherPat
        ( (((":" | "::") as prefix), _),
          [ G.Name (G.IdQualified { name_last = (id, _); _ }) ] ) ->
        let s, tok = id in
        Some
          (Taint.Ofld
             (mk_field (prefix ^ String_.strip_wrapping_char ':' s, tok)))
    | _ -> None
  in
  let unwrap_map_pair (p : G.pattern) : G.pattern =
    match p with
    | G.OtherPat
        ( (("MapPairKeyword" | "MapPairArrow"), _),
          [ G.P (G.PatKeyVal (_, _) as inner) ] ) ->
        inner
    | _ -> p
  in
  let is_map_pair (p : G.pattern) : bool =
    match p with
    | G.PatKeyVal (_, _) -> true
    | G.OtherPat
        ( (("MapPairKeyword" | "MapPairArrow"), _),
          [ G.P (G.PatKeyVal (_, _)) ] ) ->
        true
    | _ -> false
  in
  let rec go offset pat acc =
    match pat with
    | G.PatId (id, id_info) ->
        let il_name = AST_to_IL.var_of_id_info id id_info in
        (il_name, List.rev offset) :: acc
    | G.PatTyped (inner, _) -> go offset inner acc
    | G.PatTuple (_, pats, _) | G.PatList (_, pats, _)
      when pats <> [] && List.for_all is_map_pair pats ->
        (* Map/dict destructure: project by key, not by position. *)
        List.fold_left (go_map_pair offset) acc pats
    | G.PatTuple (_, pats, _) | G.PatList (_, pats, _) ->
        (* Positional list/tuple destructure with optional trailing
         * rest. Mirrors [AST_to_IL.pattern]'s positional lowering: each
         * non-rest slot seeds its leaves with [Oint i]; a trailing
         * Clojure [&] or Elixir [|] constructor seeds the rest leaves
         * with [Oslice k] so the engine projects positions [k..] of the
         * caller's argument into the rest binding. *)
        let rec emit acc i = function
          | [] -> acc
          (* Clojure: [a b & rest] → trailing PatConstructor("&", [r])
           * with a single argument; rest covers positions [i..]. Only
           * Clojure assigns this meaning to "&" inside a PatList. *)
          | [ G.PatConstructor (G.Id (("&", _), _), [ rest_pat ]) ]
            when lang =*= Lang.Clojure ->
              go (Taint.Oslice i :: offset) rest_pat acc
          (* Elixir: [a, b | t] → trailing PatConstructor("|", [b; t])
           * carrying one final fixed slot at index [i] plus a tail at
           * [i+1..]. Only Elixir uses "|" as the cons-pattern marker
           * inside a PatList. *)
          | [
           G.PatConstructor
             (G.Id (("|", _), _), [ last_fixed; tail_pat ]);
          ]
            when lang =*= Lang.Elixir ->
              let acc = go (Taint.Oint i :: offset) last_fixed acc in
              go (Taint.Oslice (i + 1) :: offset) tail_pat acc
          (* JS/TS: [a, b, ...rest] → trailing PatConstructor("...", [r]);
           * rest covers positions [i..]. *)
          | [ G.PatConstructor (G.Id (("...", _), _), [ rest_pat ]) ]
            when Lang.is_js lang ->
              go (Taint.Oslice i :: offset) rest_pat acc
          | p :: rest ->
              let acc = go (Taint.Oint i :: offset) p acc in
              emit acc (i + 1) rest
        in
        emit acc 0 pats
    | G.PatConstructor (_, pats)
      when pats <> [] && List.for_all is_map_pair pats ->
        (* Clojure [:keys] / [Assoc] destructure wraps its key-value
         * pairs in a [PatConstructor]; treat like a map destructure. *)
        List.fold_left (go_map_pair offset) acc pats
    | G.PatRecord (_, fields, _) ->
        List.fold_left
          (fun acc (dot_ident, p) ->
            match List.rev dot_ident with
            | [] -> acc
            | last :: _ -> go (Taint.Ofld (mk_field (stripped last)) :: offset) p acc)
          acc fields
    | G.PatAs (inner, (alias_id, alias_id_info)) ->
        let alias_name = AST_to_IL.var_of_id_info alias_id alias_id_info in
        let acc = (alias_name, List.rev offset) :: acc in
        go offset inner acc
    | _ -> acc
  and go_map_pair offset acc pat =
    match unwrap_map_pair pat with
    | G.PatKeyVal (key_pat, val_pat) -> (
        match ofld_of_key_pat key_pat with
        | Some k -> go (k :: offset) val_pat acc
        | None -> acc (* opaque key — skip this leaf *))
    | _ -> acc
  in
  List.rev (go [] pat [])

let mk_lambda_in_env env lcfg =
  (* We do some processing of the lambda parameters but it's mainly
   * to enable taint propagation, e.g.
   *
   *     obj.do_something(lambda x: sink(x))
   *
   * so we can propagate taint from `obj` to `x`.
   *)
  (* Clear [active_guards] at the lambda boundary: the enclosing
   * function's guards are parameter-indexed into the enclosing
   * function, and would be mis-interpreted if they rode along into the
   * lambda's own parameter scope. The enclosing function's active
   * guards are re-applied to the lambda's upflowed effects at
   * [do_lambdas], where the parameter anchoring is correct. *)
  let base_env = Lval_env.clear_active_guards env.lval_env in
  let lval_env =
    lcfg.params
    |> Fold_IL_params.fold_top_level
         (fun lval_env id id_info _pdefault ->
           let var = AST_to_IL.var_of_id_info id id_info in
           (* This is a *new* variable, so we clean any taint that we may
            * have attached to it previously. This can happen when a
            * lambda is called inside a loop. *)
           let lval_env =
             Lval_env.clean env.taint_inst.lang lval_env (LV.lval_of_var var)
           in
           (* Now check if the parameter is itself a taint source. *)
           let taints, shape, lval_env =
             check_tainted_var { env with lval_env } var
           in
           lval_env
           |> Lval_env.add_lval_shape env.taint_inst.lang (LV.lval_of_var var)
                taints shape)
         base_env
  in
  (* Destructuring ParamPatterns: the top-level pass above seeded only
   * the implicit binder. Each leaf inside the pattern also needs its
   * own env entry so body references to destructured names pick up
   * taint. Seed each leaf with both a [Var (BArg taint_arg, offset)]
   * taint (so body references propagate the caller's taint
   * conservatively when no structural shape is available) and an
   * [Arg (taint_arg, offset_path)] shape (so the shape system can
   * project the caller's actual argument down to the leaf at HOF
   * call-site instantiation). Also merge in any source taints from
   * rules that focus on leaf positions via [check_tainted_var]. *)
  let _, lval_env =
    lcfg.params
    |> List.fold_left
         (fun (i, lval_env) param ->
           match param with
           | IL.ParamPattern ({ pname; _ }, pat) ->
               let taint_arg : Taint.arg =
                 { name = fst pname.ident; index = i }
               in
               let lval_env =
                 pattern_leaves_with_offsets ~lang:env.taint_inst.lang pat
                 |> List.fold_left
                      (fun lval_env (leaf_name, offset) ->
                        let leaf_lval : IL.lval =
                          { base = Var leaf_name; rev_offset = [] }
                        in
                        let lval_env =
                          Lval_env.clean env.taint_inst.lang lval_env leaf_lval
                        in
                        let source_taints, _shape, lval_env =
                          check_tainted_var { env with lval_env } leaf_name
                        in
                        let leaf_shape = S.Arg (T.Param taint_arg, [ offset ]) in
                        let leaf_taint_lval : T.lval =
                          { base = BArg taint_arg; offset }
                        in
                        let leaf_taint =
                          T.(taint_of_orig (Var leaf_taint_lval))
                        in
                        let leaf_taints =
                          T.Taint_set.add_taint leaf_taint source_taints
                        in
                        Lval_env.add_lval_shape env.taint_inst.lang leaf_lval
                          leaf_taints leaf_shape lval_env)
                      lval_env
               in
               (i + 1, lval_env)
           | IL.Param _
           | IL.ParamRest _
           | IL.ParamKwd _
           | IL.ParamFixme ->
               (i + 1, lval_env)
           (* Receivers aren't call-site args: no env update, no arg slot. *)
           | IL.ParamReceiver _ -> (i, lval_env))
         (0, lval_env)
  in
  lval_env

(* At [TrueNode] / [FalseNode], if [cond] evaluates to a constant boolean
 * that contradicts the branch direction, the branch is unreachable. Mark
 * the env as dead via [Lval_env.mark_dead]. The dead env is discarded at
 * the Join with the live branch ([Lval_env.union] keeps the live side), so
 * anything observed in the unreachable region does not survive past it;
 * [record_effects] additionally short-circuits while the env is dead,
 * suppressing findings and signature effects recorded inside the region. *)
let prune_branch_if_unreachable (lang : Lang.t) (cond : IL.exp)
    (branch_direction : bool) (in' : Lval_env.t) : Lval_env.t =
  let eval_env = Eval_il_partial.mk_env lang Var_env.VarMap.empty in
  match Eval_il_partial.eval eval_env cond with
  | G.Lit (G.Bool (b, _)) when not (Bool.equal b branch_direction) ->
      Lval_env.mark_dead in'
  | _ -> in'

(* Guards for a branch condition ([Effect_guard.of_branch_cond] converts
 * the cond to DNF and splits a single conjunction into one guard per
 * literal, so the active-guard set tracks atoms individually and
 * reassignment drops exactly the affected atoms), gated by the
 * experimental [effect_guards] option: with the option off, only Clojure
 * keeps guards whose every literal is a [length(x) <cmp> n] atom — the
 * shape multi-arity dispatch compiles to — and no other guard is created.
 * The machinery downstream then never runs ([add_guards] no-ops on an
 * empty active set; composition and instantiation short-circuit on
 * [top]). *)
let recognised_guards (env : env) ~(negated : bool) (params : IL.param list)
    (cond : IL.exp) : Effect_guard.t list =
  if env.taint_inst.options.effect_guards then
    Effect_guard.of_branch_cond ~lang:env.taint_inst.lang
      env.shared_tables.guard_atoms ~negated params cond
  else if Lang.equal env.taint_inst.lang Lang.Clojure then
    Effect_guard.of_branch_cond ~lang:env.taint_inst.lang
      env.shared_tables.guard_atoms ~negated params cond
    |> List.filter (fun (g : Effect_guard.t) ->
           (g.cond :> Effect_guard.clause list)
           |> List.for_all (fun clause ->
                  clause
                  |> List.for_all (fun (l : Effect_guard.literal) ->
                         Effect_guard.is_length_atom l.atom.node)))
  else []

let rec transfer : env -> fun_cfg:F.fun_cfg -> Lval_env.t D.transfn =
 fun enter_env ~fun_cfg
     (* the transfer function to update the mapping at node index ni *)
       mapping ni ->
  let flow = fun_cfg.cfg in
  (* DataflowX.display_mapping flow mapping show_tainted; *)
  let in' : Lval_env.t =
    input_env ~lang:enter_env.taint_inst.lang ~enter_env:enter_env.lval_env
      ~flow mapping ni
  in
  let node = flow.graph#nodes#assoc ni in
  let env = { enter_env with lval_env = in' } in
  let out' : Lval_env.t =
    match node.F.n with
    | NInstr x ->
        let taints, shape, lval_env' = check_tainted_instr env x in
        let opt_lval = LV.lval_of_instr_opt x in
        let lval_env' =
          match opt_lval with
          | Some lval ->
              (* We call `check_tainted_lval` here because the assigned `lval`
               * itself could be annotated as a source of taint. *)
              let taints, lval_shape, _sub, lval_env' =
                check_tainted_lval { env with lval_env = lval_env' } lval
              in
              (* We check if the instruction is a sink, and if so the taints
               * from the `lval` could make a finding. *)
              check_orig_if_sink env x.iorig taints lval_shape;
              lval_env'
          | None -> lval_env'
        in
        (* Tag the written taints with the guards active at this write, so a
         * [ToLval] effect synthesised for this parameter at function exit can
         * carry the condition under which the write happens (see
         * [effects_from_arg_updates_at_exit]). Outside a guarded branch
         * [live_guards] is empty and [conjoin_guard] of [top] is identity. *)
        let taints =
          let write_guard =
            Effect_guard.conjoin
              (Effect_guard.Set.elements (Lval_env.live_guards in'))
          in
          Taints.conjoin_guard write_guard taints
        in
        let out_lval_env =
          match opt_lval with
          | Some lval ->
              if Shape.taints_and_shape_are_relevant taints shape then
                (* Instruction returns tainted data, add taints to lval.
                 * See [Taint_lval_env] for details. *)
                lval_env'
                |> Lval_env.add_lval_shape env.taint_inst.lang lval taints
                     shape
              else
                (* The RHS returns no taint, but taint could propagate by
                 * side-effect too. So, we check whether the taint assigned
                 * to 'lval' has changed to determine whether we need to
                 * clean 'lval' or not. *)
                let lval_taints_changed =
                  not
                    (Lval_env.equal_by_lval env.taint_inst.lang in' lval_env'
                       lval)
                in
                if lval_taints_changed then
                  (* The taint of 'lval' has changed, so there was a source or
                   * sanitizer acting by side-effect on this instruction. Thus we do NOT
                   * do anything more here. *)
                  lval_env'
                else
                  (* No side-effects on 'lval', and the instruction returns safe data,
                   * so we assume that the assigment acts as a sanitizer and therefore
                   * remove taints from lval. See [Taint_lval_env] for details. *)
                  Lval_env.clean env.taint_inst.lang lval_env' lval
          | None ->
              (* Instruction returns 'void' or its return value is ignored. *)
              lval_env'
        in
        (* Record the assigned variable as reassigned, so a guard that reads
         * it is dropped at stamp time: the IL is non-SSA, so a later read of
         * this name may observe a different value than a guard established
         * earlier on the path assumed. *)
        (match opt_lval with
        | Some { IL.base = IL.Var name; rev_offset = [] }
          when List.exists
                 (function
                   | IL.Param ({ pname; by_reference = false; _ } as np)
                   | IL.ParamKwd ({ pname; by_reference = false; _ } as np) ->
                       IL.equal_name pname name
                       && not
                            (param_is_copy
                               ~is_value_type:env.taint_inst.is_value_type np)
                   | _ -> false)
                 fun_cfg.params ->
            effects_before_param_rebinding ~lang:env.taint_inst.lang
              enter_env.lval_env in' name
            |> record_effects env
        | _ -> ());
        let out_lval_env =
          match (opt_lval, x.i) with
          | ( Some { IL.base = IL.Var name; rev_offset = [] },
              CallSpecial (_, (IL.Ref, _), [ IL.Unnamed { e = Fetch target; _ } ])
            ) ->
              Lval_env.set_pointee env.taint_inst.lang name target out_lval_env
          | ( Some { IL.base = IL.Var name; rev_offset = [] },
              Assign (_, { e = Fetch { base = Var src; rev_offset = [] }; _ }) )
            ->
              Lval_env.copy_pointees ~src ~dst:name out_lval_env
          | Some { IL.base = IL.Var name; rev_offset = [] }, _ ->
              Lval_env.forget_pointees name out_lval_env
          | _ -> out_lval_env
        in
        (match opt_lval with
        | Some { IL.base = IL.Var name; _ } ->
            Lval_env.mark_reassigned name out_lval_env
        | _ -> out_lval_env)
    | NCond (_tok, e)
    | NThrow (_tok, e) ->
        let _taints, _shape, lval_env' = check_tainted_expr env e in
        lval_env'
    | NReturn (tok, e) ->
        (* TODO: Move most of this to check_tainted_return. *)
        let taints, shape, lval_env' = check_tainted_return env tok e in
        let effects =
          effects_of_tainted_return env
            ~several_results:
              (returns_several_results env.taint_inst.lang fun_cfg e)
            taints shape tok
        in
        record_effects env effects;
        lval_env'
    | TrueNode cond ->
        let pruned =
          prune_branch_if_unreachable env.taint_inst.lang cond true in'
        in
        recognised_guards env ~negated:false fun_cfg.params cond
        |> List.fold_left
             (fun lval_env g ->
               Log.debug (fun m ->
                   m "GUARD_STAMP: TrueNode adds %s" (Effect_guard.show g));
               Lval_env.add_active_guard g lval_env)
             pruned
    | FalseNode cond ->
        let pruned =
          prune_branch_if_unreachable env.taint_inst.lang cond false in'
        in
        recognised_guards env ~negated:true fun_cfg.params cond
        |> List.fold_left
             (fun lval_env g ->
               Log.debug (fun m ->
                   m "GUARD_STAMP: FalseNode adds %s" (Effect_guard.show g));
               Lval_env.add_active_guard g lval_env)
             pruned
    | NGoto _
    | Enter
    | Exit
    | Join
    | NOther _
    | NTodo _ ->
        in'
  in
  let effects_lambdas, out' =
    do_lambdas { env with lval_env = out' } fun_cfg.lambdas node
  in
  env.effects_acc := Effects.union effects_lambdas !(env.effects_acc);
  let env_at_exit = { env with lval_env = out' } in
  check_tainted_control_at_exit node env_at_exit;
  Log.debug (fun m ->
      m ~tags:transfer_tag "Taint transfer %s%s\n  %s:\n  IN:  %s\n  OUT: %s"
        (Option.map IL.str_of_name env.func.name ||| "<FUN>")
        (Option.map
           (fun lname -> spf "(in lambda %s)" (IL.str_of_name lname))
           env.in_lambda
        ||| "")
        (Display_IL.short_string_of_node_kind node.F.n)
        (Lval_env.to_string in') (Lval_env.to_string out'));
  { D.in_env = in'; out_env = out' }

(* In OSS, lambdas are mostly treated like statement blocks, that is, we
 * check the body of the lambda at the place where it is called, but we
 * do not "connect" actual arguments with formals, nor we track if the
 * lambda returns any taint.
 *
 * TODO: In Pro we should do inter-procedural analysis here. *)
and do_lambdas env (lambdas : IL.lambdas_cfgs) node =
  let node_is_call =
    (* See 'out_env' below. *)
    match node.F.n with
    | NInstr i -> (
        match i.i with
        | Call _
        | CallSpecial _
        | New _ ->
            true
        | Assign _
        | AssignAnon _
        | FixmeInstr _ ->
            false)
    | __else__ -> false
  in
  (* We visit lambdas at their "use" site (where they are fetched), so we can e.g.
   * propagate taint from an object receiving a method call, to a lambda being
   * passed to that method. *)
  let lambdas_to_analyze = lambdas_to_analyze_in_node env lambdas node in
  Log.debug (fun m ->
      match List.length lambdas_to_analyze with
      | 0 -> ()
      | num_lambdas ->
          m "There are %d lambda(s) occurring in: %s" num_lambdas
            (Display_IL.short_string_of_node_kind node.F.n));
  let effects_lambdas, out_envs_lambdas =
    lambdas_to_analyze
    |> List_.map (fun (lambda_name, lambda_cfg) ->
           let lambda_in_env = mk_lambda_in_env env lambda_cfg in
           fixpoint_lambda env.taint_inst env.shared_tables env.func env.needed_vars lambda_name
             lambda_cfg lambda_in_env ?signature_db:env.signature_db
             ?builtin_signature_db:env.builtin_signature_db ())
    |> List_.split
  in
  let effects = Effects.union_list effects_lambdas in
  (* Restamp the lambda's upflowed effects with the enclosing function's
   * currently-active guards — the guards in scope at the node where the
   * lambda is being evaluated. Paired with [mk_lambda_in_env]'s
   * [clear_active_guards], this keeps the invariant that every effect in
   * a function's signature references only that function's own
   * parameters: a lambda's own guards stay within the lambda (stamped on
   * effects inside the lambda's own fixpoint), and the enclosing
   * function's guards are applied here at the upflow boundary where the
   * parameter anchoring is the enclosing function's. *)
  let effects =
    let active = Lval_env.live_guards env.lval_env in
    if Effect_guard.Set.is_empty active then effects
    else
      let g = Effect_guard.conjoin (Effect_guard.Set.elements active) in
      Effects.map (Effect.add_guards g) effects
  in
  let out_env =
    if node_is_call then
      (* We only take the side-effects of the lambda into consideration if the
       * node is a call, so the lambda is either the callee or one of its arguments.
       * E.g.
       *
       *     do_something([]() { taint(p) });
       *     sink(p) // finding wanted
       *
       * We assume that these lambdas are being evaluated and that their side-effects
       * should affect the subsequent statements.
       *)
      Lval_env.union_list ~lang:env.taint_inst.lang ~default:env.lval_env
        out_envs_lambdas
    else
      (* If lambdas are not part of a call, we don't make their side-effects visible.
       * E.g.
       *
       *     void test(int *p) {
       *       auto f1 = [&p]() {
       *         source(p);
       *       };
       *       auto f2 = [&p]() {
       *         sink(p); // NO finding wanted
       *       };
       *     }
       *)
      env.lval_env
  in
  (effects, out_env)

and fixpoint_lambda taint_inst shared_tables func needed_vars lambda_name lambda_cfg in_env
    ?signature_db ?builtin_signature_db () :
    Effects.t * Lval_env.t =
  Log.debug (fun m ->
      m "Analyzing lambda %s (%s)"
        (IL.str_of_name lambda_name)
        (Lval_env.to_string in_env));
  let effects, mapping =
    fixpoint_aux taint_inst shared_tables func ~needed_vars ~enter_lval_env:in_env
      ~in_lambda:(Some lambda_name) ?signature_db
      ?builtin_signature_db lambda_cfg
  in
  let effects =
    effects
    |> Effects.filter (function
         | ToSink _
         | ToLval _
         | ToSinkInCall _ ->
             true
         | ToReturn _ -> false)
  in
  let out_env = mapping.(lambda_cfg.cfg.exit).Dataflow_core.out_env in
  let out_env' =
    out_env
    |> Lval_env.filter_tainted (fun var ->
           (* Always preserve instance variables by checking if they were created from VarSpecial *)
           (* We need access to the original lval structure, not just the normalized name *)
           (* For now, keep the original logic but we'll need to fix the normalization *)
           IL.NameSet.mem var needed_vars)
  in
  Log.debug (fun m ->
      m ~tags:transfer_tag "Lambda out_env %s --FILTER(%s)--> %s"
        (Lval_env.to_string out_env)
        (IL.NameSet.show needed_vars)
        (Lval_env.to_string out_env'));
  (effects, out_env')

and fixpoint_aux taint_inst shared_tables func ?(needed_vars = IL.NameSet.empty)
    ~enter_lval_env ~in_lambda ?signature_db ?builtin_signature_db fun_cfg =
  let flow = fun_cfg.cfg in
  let init_mapping = DataflowX.new_node_array flow Lval_env.empty_inout in
  let needed_vars =
    needed_vars
    |> IL.NameSet.union
         (Taint_lambdas.find_vars_to_track_across_lambdas fun_cfg)
  in
  let env =
    {
      taint_inst;
      shared_tables;
      func;
      in_lambda;
      lval_env = enter_lval_env;
      needed_vars;
      effects_acc = ref Effects.empty;
      did_self_recurse = ref false;
      signature_db;
      builtin_signature_db;
    }
  in
  (* THINK: Why I cannot just update mapping here ? if I do, the mapping gets overwritten later on! *)
  (* dump CFG while debugging *)
  (*
    Printf.printf "[CFG] dump for %s\n%!"
      (Option.map IL.str_of_name env.func.name ||| "<anon>");
    flow.graph#nodes#tolist
    |> List.iter (fun (ni, node) ->
           if CFG.NodeiSet.mem ni flow.reachable then (
             Printf.printf "  node %3d: %s\n%!" ni
               (Display_IL.short_string_of_node_kind node.F.n);
             let succs =
               flow.graph#successors ni
               |> fun s -> s#tolist |> List.map fst
             in
             Printf.printf "           -> %s\n%!"
               (succs |> List.map string_of_int |> String.concat ", ")))
  ;
  *)
  let base_timeout =
    Common.(
      taint_inst.options.taint_fixpoint_timeout
      ||| Limits_semgrep.taint_FIXPOINT_TIMEOUT)
  in
  (* Interfile runs many more functions per fixpoint; scale the timeout up to avoid false timeouts. *)
  let interfile_timeout_multiplier = 20.0 in
  let timeout =
    if taint_inst.options.taint_intrafile then
      base_timeout *. interfile_timeout_multiplier
    else base_timeout
  in
  (* The inner [DataflowX.fixpoint] converges on per-node [lval_env]
   * stability, but not on [effects_acc] — the latter is function-global
   * monotonic state that grows as the body records taint effects. Direct
   * self-recursive calls need to see effects recorded by earlier passes via
   * [self_sig_if_recursive]. We wrap the inner fixpoint in an outer loop
   * that re-runs only if a self-recursive call happened AND the effects set
   * grew, terminating when stable (or at a safety cap).
   *
   * Gated by [needs_self_sig_fixpoint]: only languages where self-sig
   * lifting can yield outcomes that body-direct effect recording would
   * miss. Today that's just Clojure (arity-guarded multi-arity dispatch);
   * other languages rely on direct recording and the outer loop would be a
   * wasted pass. *)
  let needs_self_sig_fixpoint =
    match taint_inst.lang with Lang.Clojure -> true | _ -> false
  in
  let end_mapping, timeout_status =
    if needs_self_sig_fixpoint then
      let rec run_to_sig_fixpoint passes =
        let prev_effects = !(env.effects_acc) in
        env.did_self_recurse := false;
        let end_mapping, status =
          DataflowX.fixpoint ~timeout ~eq_env:Lval_env.equal ~init:init_mapping
            ~trans:(transfer env ~fun_cfg) ~forward:true ~flow
        in
        (* Cheap checks first; only compute the stabilisation test (a set
         * comparison) when neither short-circuits. [equal_with_guards], not
         * [equal]: a pass that only fuses a new disjunct into an existing
         * effect's guard must count as growth, or the loop would stop with
         * the narrower guard and drop effects the refined guard keeps. *)
        if not !(env.did_self_recurse) then (end_mapping, status)
        else if passes >= Limits_semgrep.taint_MAX_SELF_SIG_PASSES then (
          (* Hit the pass cap while still self-recursing. If the effects also
           * stopped growing this pass it is a clean fixpoint; otherwise the
           * result under-approximates (possible false negatives), so surface
           * it rather than truncating silently — the inner fixpoint timeout
           * is reported the same way by [log_timeout_warning]. *)
          if not (Effects.equal_with_guards prev_effects !(env.effects_acc))
          then
            (* nosemgrep: no-logs-in-library *)
            Logs.warn (fun m ->
                m
                  "Self-signature fixpoint hit the %d-pass cap with effects \
                   still growing; result may under-approximate [rule: %s \
                   file: %s func: %s]"
                  Limits_semgrep.taint_MAX_SELF_SIG_PASSES
                  (Rule_ID.to_string taint_inst.rule_id)
                  !!(taint_inst.file)
                  (Option.map IL.str_of_name env.func.name ||| "???"));
          (end_mapping, status))
        else if Effects.equal_with_guards prev_effects !(env.effects_acc) then
          (end_mapping, status)
        else run_to_sig_fixpoint (passes + 1)
      in
      run_to_sig_fixpoint 0
    else
      DataflowX.fixpoint ~timeout ~eq_env:Lval_env.equal ~init:init_mapping
        ~trans:(transfer env ~fun_cfg) ~forward:true ~flow
  in
  log_timeout_warning taint_inst env.func.name timeout_status;
  let exit_lval_env = end_mapping.(flow.exit).D.out_env in
  let rebound = rebound_vars fun_cfg.cfg in
  let copied =
    copied_params
      ~is_value_type:taint_inst.is_value_type fun_cfg.params
  in
  effects_from_arg_updates_at_exit ~lang:taint_inst.lang
    ~params:fun_cfg.params ~rebound ~copied enter_lval_env exit_lval_env
  |> record_effects env;
  let effects =
    if Effects.exists effect_has_closure_env !(env.effects_acc) then
      convert_escaping_closures ~fun_cfg
        ~captured:(Lazy.force env.func.captured) ~rebound ~copied exit_lval_env
        !(env.effects_acc)
    else !(env.effects_acc)
  in
  (effects, end_mapping)

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

and (fixpoint :
      Taint_rule_inst.t ->
      Taint_shared_tables.t ->
      ?in_env:Lval_env.t ->
      ?name:IL.name ->
      ?class_name:string ->
      ?signature_db:Shape_and_sig.signature_database ->
      ?builtin_signature_db:Shape_and_sig.builtin_signature_database ->
      F.fun_cfg ->
      Effects.t * mapping) =
 fun taint_inst shared_tables ?(in_env = Lval_env.empty) ?name ?class_name
     ?signature_db
     ?builtin_signature_db fun_cfg ->
  let taint_intrafile_ = taint_inst.options.taint_intrafile in
  (* Check if this is a constructor and get class-level instance variable taint *)
  let enhanced_in_env =
    if taint_intrafile_ then
      match name with
      | Some func_name_node -> (
          let func_name = fst func_name_node.IL.ident in
          let is_ctor = Object_initialization.is_constructor taint_inst.lang func_name class_name in
          if is_ctor then in_env
          else
            (* This is not a constructor, check if we have stored instance variable taint *)
            match class_name with
            | Some cls -> (
                let storage_key =
                  Printf.sprintf "%s:%s" (Fpath.to_string taint_inst.file) cls
                in
                try
                  let class_instance_vars =
                    Hashtbl.find
                      shared_tables.Taint_shared_tables.constructor_envs
                      storage_key
                  in
                  Lval_env.union ~lang:taint_inst.lang in_env
                    class_instance_vars
                with
                | Not_found -> in_env)
            | None ->
                in_env (* Don't look for instance taint when no class context *)
          )
      | None -> in_env
    else in_env
  in
  (* Extract signatures for all lambdas in the function for HOF support.
     We collect ALL lambdas (including nested ones) in innermost-first order,
     so nested lambda signatures are available when processing their parents. *)
  let best_matches =
    (* Here we compute the "canonical" or "best" source/sanitizer/sink matches,
     * for each source/sanitizer/sink we check whether there is a "best match"
     * among all the potential matches in the CFG.
     * See NOTE "Best matches" *)
    fun_cfg
    |> TM.best_matches_in_nodes ~sub_matches_of_orig:(fun orig ->
           let sources =
             orig_is_source taint_inst orig
             |> List.to_seq
             |> Seq.filter (fun (m : R.taint_source TM.t) ->
                    m.spec.source_exact)
             |> Seq.map (fun m -> TM.Any m)
           in
           let sanitizers =
             orig_is_sanitizer taint_inst orig
             |> List.to_seq
             |> Seq.filter (fun (m : R.taint_sanitizer TM.t) ->
                    m.spec.sanitizer_exact)
             |> Seq.map (fun m -> TM.Any m)
           in
           let sinks =
             orig_is_sink taint_inst orig
             |> List.to_seq
             |> Seq.filter (fun (m : R.taint_sink TM.t) -> m.spec.sink_exact)
             |> Seq.map (fun m -> TM.Any m)
           in
           sources |> Seq.append sanitizers |> Seq.append sinks)
  in
  let used_lambdas = lambdas_used_in_cfg fun_cfg in
  let func =
    {
      name;
      sig_params = Signature_params.of_IL_params fun_cfg.params;
      il_params = fun_cfg.params;
      param_sids = mk_param_sids fun_cfg.params;
      captured = lazy (captured_of_fun_cfg fun_cfg);
      best_matches;
      used_lambdas;
    }
  in
  (* The lambdas whose signature can be consumed: a [Fun] shape is only
     read when the lambda is handed to a callee that has a signature (so
     [Sig_inst] may instantiate it), called through its variable, or used
     in any other way (kept, conservatively).  A lambda only ever passed to
     callees without a signature -- RSpec's [describe]/[it] blocks, Rails
     DSL blocks -- never has its signature looked up, and extracting it (a
     dataflow per lambda per enclosing fixpoint, nested blocks repeatedly)
     was most of the interfile epilogue on GitLab. *)
  let needed_lambdas (all_lambdas : (IL.name * IL.fun_cfg) list) : IL.NameSet.t
      =
    let lambda_names =
      List.fold_left
        (fun s (n, _) -> IL.NameSet.add n s)
        IL.NameSet.empty all_lambdas
    in
    if IL.NameSet.is_empty lambda_names then lambda_names
    else
      let probe_env =
        {
          taint_inst;
          shared_tables;
          func;
          in_lambda = None;
          needed_vars = IL.NameSet.empty;
          lval_env = enhanced_in_env;
          effects_acc = ref Effects.empty;
          did_self_recurse = ref false;
          signature_db;
          builtin_signature_db;
        }
      in
      let callee_has_sig_memo : bool Callee_resolution.Callee_use_tbl.t =
        Callee_resolution.Callee_use_tbl.create 16
      in
      let callee_has_sig (callee : IL.exp) (args : IL.exp IL.argument list) :
          bool =
        let lookup () : bool =
          not
            (List.is_empty
               (lookup_signature probe_env callee (List.length args)))
        in
        match callee_use callee with
        | None -> lookup ()
        | Some use -> (
            let key = (use, Some (argument_types taint_inst.Taint_rule_inst.lang args)) in
            match
              Callee_resolution.Callee_use_tbl.find_opt callee_has_sig_memo key
            with
            | Some b -> b
            | None ->
                let b = lookup () in
                Callee_resolution.Callee_use_tbl.replace callee_has_sig_memo key
                  b;
                b)
      in
      let lambda_var_of_lval (lv : IL.lval) : IL.name option =
        match lv with
        | { base = Var v; rev_offset = [] } when IL.NameSet.mem v lambda_names ->
            Some v
        | _ -> None
      in
      let lambda_var_of_exp (e : IL.exp) : IL.name option =
        match e.e with
        | Fetch lv -> lambda_var_of_lval lv
        | _ -> None
      in
      let count v xs =
        List.length
          (List.filter (fun x -> Int.equal (IL.NameOrdered.compare x v) 0) xs)
      in
      LV.reachable_nodes fun_cfg
      |> Seq.fold_left
           (fun (needed : IL.NameSet.t) (node : IL.node) ->
             let mentioned =
               LV.rlvals_of_node node.n |> List.filter_map lambda_var_of_lval
             in
             match mentioned with
             | [] -> needed
             | _ -> (
                 match node.n with
                 | NInstr { i = Call (_, callee, args); _ } ->
                     let bare_args =
                       List.filter_map
                         (function
                           | Unnamed e | Named (_, e) -> lambda_var_of_exp e)
                         args
                     in
                     let callee_needs = callee_has_sig callee args in
                     List.fold_left
                       (fun needed v ->
                         (* needed unless its only uses here are as a bare
                            argument to a callee without a signature *)
                         if
                           (not callee_needs)
                           && Int.equal (count v mentioned) (count v bare_args)
                         then needed
                         else IL.NameSet.add v needed)
                       needed mentioned
                 | _ ->
                     List.fold_left
                       (fun needed v -> IL.NameSet.add v needed)
                       needed mentioned))
           IL.NameSet.empty
  in
  let signature_db_with_lambdas =
    Taint_timing.accum "eager lambda signature extraction" @@ fun () ->
    if taint_intrafile_ then
      match signature_db with
      | Some db ->
          (* Collect all lambdas recursively, innermost first *)
          let all_lambdas_list = collect_all_lambdas_innermost_first fun_cfg in
          let needed = needed_lambdas all_lambdas_list in
          let all_lambdas_list =
            List.filter
              (fun (n, _) -> IL.NameSet.mem n needed)
              all_lambdas_list
          in
          List.fold_left
            (fun acc_db (lambda_name, lambda_cfg) ->
              let fn_id = Function_id.of_il_name lambda_name in
              if Shape_and_sig.FunctionMap.mem fn_id acc_db.Shape_and_sig.signatures then
                (* An earlier extraction (e.g. [Match_tainting_mode]'s outer
                   extraction for top-level lambdas) already added a sig at
                   this key. Adding here would put a second sig in the same
                   [SignatureSet], which [find_by_arity] cannot disambiguate
                   when both share an arity. *)
                acc_db
              else
              try
                   Log.debug (fun m ->
                       m "Extracting signature for lambda %s"
                         (IL.str_of_name lambda_name));
                   let params = Signature_params.of_IL_params lambda_cfg.params in
                   (* Create assumptions for lambda parameters using Fold_IL_params.
                    * [fold_top_level] yields one entry per declared parameter;
                    * the loop index [i] must line up with actual call-site args,
                    * which rules out enumerating destructured leaves here. *)
                   let param_assumptions =
                     let _, env =
                       lambda_cfg.params
                       |> List.fold_left
                            (fun (i, env) param ->
                              match param with
                              | IL.Param { pname; _ }
                              | IL.ParamRest { pname; _ }
                              | IL.ParamKwd { pname; _ }
                              | IL.ParamPattern ({ pname; _ }, _) ->
                                  let var = pname in
                                  let il_lval : IL.lval =
                                    { base = Var var; rev_offset = [] }
                                  in
                                  let taint_arg : Taint.arg =
                                    { name = fst var.ident; index = i }
                                  in
                                  let taint_lval : Taint.lval =
                                    { base = BArg taint_arg; offset = [] }
                                  in
                                  let generic_taint =
                                    Taint.(taint_of_orig (Var taint_lval))
                                  in
                                  let taint_set =
                                    Taint.Taint_set.singleton generic_taint
                                  in
                                  (* Give the parameter an Arg shape so it can be used in HOF *)
                                  let param_shape = S.Arg (T.Param taint_arg, [ [] ]) in
                                  let env =
                                    Lval_env.add_lval_shape taint_inst.lang
                                      il_lval taint_set param_shape env
                                  in
                                  (* Destructuring ParamPattern: also seed
                                   * each leaf with both a
                                   * [Var (BArg taint_arg, offset)] taint
                                   * (so body references propagate taint
                                   * conservatively) and an
                                   * [Arg (taint_arg, offset_path)] shape
                                   * (so the shape system can project the
                                   * caller's actual argument down to the
                                   * leaf at HOF call-site instantiation). *)
                                  let env =
                                    match param with
                                    | IL.ParamPattern (_, pat) ->
                                        pattern_leaves_with_offsets
                                          ~lang:taint_inst.lang pat
                                        |> List.fold_left
                                             (fun env (leaf_name, offset) ->
                                               let leaf_lval : IL.lval =
                                                 {
                                                   base = Var leaf_name;
                                                   rev_offset = [];
                                                 }
                                               in
                                               let leaf_shape =
                                                 S.Arg (T.Param taint_arg, [ offset ])
                                               in
                                               let leaf_taint_lval : Taint.lval
                                                   =
                                                 { base = BArg taint_arg; offset }
                                               in
                                               let leaf_taint =
                                                 Taint.(taint_of_orig (Var leaf_taint_lval))
                                               in
                                               let leaf_taints =
                                                 Taint.Taint_set.singleton
                                                   leaf_taint
                                               in
                                               Lval_env.add_lval_shape
                                                 taint_inst.lang leaf_lval
                                                 leaf_taints leaf_shape env)
                                             env
                                    | _ -> env
                                  in
                                  (i + 1, env)
                              | IL.ParamFixme -> (i + 1, env)
                              (* Receivers aren't call-site args. *)
                              | IL.ParamReceiver _ -> (i, env))
                            (0, Lval_env.empty)
                     in
                     env
                   in
                   let lambda_captured = captured_of_fun_cfg lambda_cfg in
                   let combined_env =
                     Lval_env.union ~lang:taint_inst.lang enhanced_in_env
                       param_assumptions
                     |> seed_captured_vars taint_inst.lang lambda_captured
                   in
                   (* Run fixpoint on lambda to get its effects *)
                   let lambda_best_matches =
                     lambda_cfg
                     |> TM.best_matches_in_nodes ~sub_matches_of_orig:(fun orig ->
                            let sources =
                              orig_is_source taint_inst orig
                              |> List.to_seq
                              |> Seq.filter (fun (m : R.taint_source TM.t) ->
                                     m.spec.source_exact)
                              |> Seq.map (fun m -> TM.Any m)
                            in
                            let sanitizers =
                              orig_is_sanitizer taint_inst orig
                              |> List.to_seq
                              |> Seq.filter (fun (m : R.taint_sanitizer TM.t) ->
                                     m.spec.sanitizer_exact)
                              |> Seq.map (fun m -> TM.Any m)
                            in
                            let sinks =
                              orig_is_sink taint_inst orig
                              |> List.to_seq
                              |> Seq.filter (fun (m : R.taint_sink TM.t) ->
                                     m.spec.sink_exact)
                              |> Seq.map (fun m -> TM.Any m)
                            in
                            sources |> Seq.append sanitizers |> Seq.append sinks)
                   in
                   let lambda_func =
                     {
                       name = Some lambda_name;
                       sig_params =
                         Signature_params.of_IL_params lambda_cfg.params;
                       il_params = lambda_cfg.params;
                       param_sids = mk_param_sids lambda_cfg.params;
                       captured = Lazy.from_val lambda_captured;
                       best_matches = lambda_best_matches;
                       used_lambdas = IL.NameSet.empty;
                     }
                   in
                   let lambda_effects, _lambda_mapping =
                     fixpoint_aux taint_inst shared_tables lambda_func
                       ~enter_lval_env:combined_env
                       ~in_lambda:(Some lambda_name)
                       ~signature_db:acc_db ?builtin_signature_db lambda_cfg
                   in
                   let signature =
                     {
                       Signature.params;
                       params_il = lambda_cfg.params;
                       captured = lambda_captured;
                       effects = drop_writes_to_own_vars lambda_cfg lambda_effects;
                     }
                   in
                   let arity =
                     Shape_and_sig.Arity_exact (List.length lambda_cfg.params)
                   in
                   Shape_and_sig.add_signature acc_db (Function_id.of_il_name lambda_name)
                     { sig_ = signature; arity }
                 with
                 | e ->
                     Log.warn (fun m ->
                         m "Failed to extract signature for lambda %s: %s"
                           (IL.str_of_name lambda_name)
                           (Printexc.to_string e));
                     acc_db)
            db all_lambdas_list
          |> Option.some
      | None -> signature_db
    else signature_db
  in

  let effects, mapping =
    Taint_timing.accum "main dataflow pass" @@ fun () ->
    fixpoint_aux taint_inst shared_tables func ~enter_lval_env:enhanced_in_env ~in_lambda:None
      ?signature_db:signature_db_with_lambdas ?builtin_signature_db fun_cfg
  in
  (* If this was a constructor, store the instance variable taint for other methods *)
  (if taint_intrafile_ then
     match name with
     | Some func_name_node -> (
         let func_name = fst func_name_node.IL.ident in
         if Object_initialization.is_constructor taint_inst.lang func_name class_name then
           match class_name with
           | Some cls ->
               (* Not the constructor's temporaries: they are numbered per
                  function, so they would alias the temporaries of the
                  methods this is unioned into. They have fake tokens. *)
               let final_env =
                 mapping.(fun_cfg.cfg.exit).Dataflow_core.out_env
                 |> Lval_env.filter_tainted (fun var ->
                        not (Tok.is_fake (snd var.IL.ident)))
               in
               let storage_key =
                 Printf.sprintf "%s:%s" (Fpath.to_string taint_inst.file) cls
               in
               Hashtbl.replace
                 shared_tables.Taint_shared_tables.constructor_envs
                 storage_key final_env
           | None -> ())
     | None -> ());
  (effects, mapping)
[@@profiling]

let fixpoint taint_inst shared_tables ?in_env ?name ?class_name ?signature_db ?builtin_signature_db fun_cfg =
  fixpoint taint_inst shared_tables ?in_env ?name ?class_name ?signature_db ?builtin_signature_db fun_cfg
[@@profiling]
