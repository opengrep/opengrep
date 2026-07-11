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

module LangOrd = struct
  type t = Lang.t

  let compare = Stdlib.compare
end

module LangMap = Map.Make (LangOrd)
module LangSet = Set.Make (LangOrd)
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
  file_ast : G.program option;  (* [Some] cross-file, [None] current file *)
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
  | Taint.PM (pm, _) ->
      let toks = Lazy.force pm.tokens |> List.filter Tok.is_origintok in
      Taint_trace.Toks toks
  | Taint.Call (expr, toks, ct) ->
      Taint_trace.Call
        {
          call_toks =
            AST_generic_helpers.ii_of_any (G.E expr)
            |> List.filter Tok.is_origintok;
          intermediate_vars = toks;
          call_trace = convert_taint_call_trace ct;
        }

(* For now CLI does not support multiple taint traces for a finding, and it
 * simply picks the _first_ trace from this list. So here we apply a number
 * of heuristics to make sure the first trace in this list is the most
 * relevant one. This is particularly important when using (experimental)
 * taint labels, because not all labels are equally relevant for the finding. *)
let sources_of_taints ?preferred_label taints =
  (* We only report actual sources reaching a sink. If users want Semgrep to
   * report function parameters reaching a sink without sanitization, then
   * they need to specify the parameters as taint sources. *)
  let taint_sources =
    taints
    |> List_.filter_map
         (fun { Effect.taint = { orig; tokens }; sink_trace; guard = _ } ->
           match orig with
           | Src src -> Some (src, tokens, sink_trace)
           (* even if there is any taint "variable", it's irrelevant for the
            * finding, since the precondition is satisfied. *)
           | Var _
           | Shape_var _
           | Control ->
               None)
  in
  let taint_sources =
    (* If there is a "preferred label", then sort sources to make sure this
       label is picked before others. See 'preferred_label_of_sink'. *)
    match preferred_label with
    | None -> taint_sources
    | Some label ->
        taint_sources
        |> List.stable_sort (fun (src1, _, _) (src2, _, _) ->
               match (src1.T.label = label, src2.T.label = label) with
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
    |> Either_.partition (fun (src, tokens, sink_trace) ->
           match get_source_requires src with
           | Some _ -> Left (src, tokens, sink_trace)
           | None -> Right (src, tokens, sink_trace))
  in
  if without_req <> [] then without_req
  else (
    Log.warn (fun m ->
        m
          "Taint source without precondition wasn't found. Displaying the \
           taint trace from the source with precondition.");
    with_req)

let trace_of_source source =
  let src, tokens, sink_trace = source in
  {
    Taint_trace.source_trace = convert_taint_call_trace src.T.call_trace;
    tokens;
    sink_trace = convert_taint_call_trace sink_trace;
  }

(* Carried guards ([Sig_inst.classify_guards] defers every non-dispatch
 * guard) are decided here, when the effect becomes a match: the effect-level
 * guard drops the whole effect if it folds to false, and each sink item's
 * guard — the guard its taint carried when it reached the sink — drops that
 * item. This runs before any match deduplication ([PM.uniq] and reporting's
 * dedup_and_sort), so a finding survives iff some candidate's guard is not
 * false. An undecided guard reports, as a guard-less effect would. *)
let guard_folds_false ~lang (g : Effect_guard.t) : bool =
  (not (Effect_guard.is_top g))
  &&
  let eval_env = Eval_il_partial.mk_env lang Dataflow_var_env.VarMap.empty in
  let eval_atom atom =
    match Eval_il_partial.eval eval_env atom with
    | AST_generic.Lit (AST_generic.Bool (b, _)) -> Some b
    | _ -> None
  in
  match Effect_guard.eval_with eval_atom g.cond with
  | Some false -> true
  | Some true
  | None ->
      false

let match_on_of_xconf (xconf : Match_env.xconfig) : [ `Sink | `Source ] =
  (* TEMPORARY HACK to support both taint_match_on (DEPRECATED) and
   * taint_focus_on (preferred name by SR). *)
  match (xconf.config.taint_focus_on, xconf.config.taint_match_on) with
  | `Source, _
  | _, `Source ->
      `Source
  | `Sink, `Sink -> `Sink

let pms_of_effect ~lang ~match_on (effect_ : Effect.t) =
  match effect_ with
  | ToLval _
  | ToReturn _
  | ToSinkInCall _ ->
      []
  | _ when guard_folds_false ~lang (Effect.guards_of effect_) -> []
  | ToSink
      {
        taints_with_precondition = taints, requires;
        sink = { pm = sink_pm; _ } as sink;
        merged_env;
        _;
      } -> (
      let taints =
        taints
        |> List.filter (fun (i : Effect.taint_to_sink_item) ->
               not (guard_folds_false ~lang i.guard))
      in
      let actual_taints = List_.map (fun t -> t.Effect.taint) taints in
      let satisfies =
        (not (List_.null taints))
        && T.taints_satisfy_requires actual_taints requires
      in
      if not satisfies then []
      else
        let preferred_label = preferred_label_of_sink sink in
        let taint_sources = sources_of_taints ?preferred_label taints in
        match match_on with
        | `Sink ->
            (* The old behavior used to be that, for sinks with a `requires`, we would
               generate a finding per every single taint source going in. Later deduplication
               would deal with it.
               We will instead choose to consolidate all sources into a single finding. We can
               do some postprocessing to report only relevant sources later on, but for now we
               will lazily (again) defer that computation to later.
            *)
            let traces = List_.map trace_of_source taint_sources in
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
            let taint_trace = Some (lazy traces) in
            [ { sink_pm with env = merged_env; taint_trace } ]
        | `Source ->
            taint_sources
            |> List_.map (fun source ->
                   let src, tokens, sink_trace = source in
                   let src_pm, _ = T.pm_of_trace src.T.call_trace in
                   let trace =
                     {
                       Taint_trace.source_trace =
                         convert_taint_call_trace src.T.call_trace;
                       tokens;
                       sink_trace = convert_taint_call_trace sink_trace;
                     }
                   in
                   {
                     src_pm with
                     env = merged_env;
                     taint_trace = Some (lazy [ trace ]);
                   }))

let pms_of_effects ~lang ~match_on (effects : Effects.t) : PM.t list =
  Effects.fold
    (fun (effect_ : Effect.t) (acc : PM.t list) ->
       List.rev_append (pms_of_effect ~lang ~match_on effect_) acc)
    effects []

(*****************************************************************************)
(* Main entry points *)
(*****************************************************************************)

(* Analyse a function from a pre-built [IL.fun_cfg]. *)
let check_fundef_with_cfg (taint_inst : Taint_rule_inst.t) (name : IL.name)
    ?glob_env ?class_name ?signature_db ?builtin_signature_db ?call_graph
    (fcfg : IL.fun_cfg) =
  let in_env, env_effects =
    Taint_input_env.mk_fun_input_env taint_inst ?glob_env fcfg.IL.params
  in
  let effects, mapping =
    Dataflow_tainting.fixpoint taint_inst ~in_env ~name ?class_name
      ?signature_db ?builtin_signature_db ?call_graph fcfg
  in
  let effects = Effects.union env_effects effects in
  (fcfg, effects, mapping)

(* [check_fundef_with_cfg] on a freshly-lowered [fdef]. *)
let check_fundef (taint_inst : Taint_rule_inst.t) (name : IL.name) ?glob_env
    ?class_name ?signature_db ?builtin_signature_db ?call_graph fdef =
  let fdef = AST_to_IL.function_definition taint_inst.lang fdef in
  check_fundef_with_cfg taint_inst name ?glob_env ?class_name ?signature_db
    ?builtin_signature_db ?call_graph (CFG_build.cfg_of_fdef fdef)

(* Implicit receiver (Go/Rust ParamReceiver, Python first method param);
   reached as [BThis] not [BArg], so stripping keeps [BArg] indices aligned. *)
let is_implicit_receiver (lang : Lang.t) ~(is_first : bool)
    ~(is_static : bool) (class_name_str : string option)
    (gparam : G.parameter) : bool =
  match lang, gparam with
  | Lang.Go, G.ParamReceiver _ -> true
  | Lang.Rust, G.ParamReceiver _ -> true
  | _ -> (
      match class_name_str with
      | None -> false
      | Some _ ->
          (* Python's receiver is the first parameter of an instance/class
             method whatever it is named ([self], [cls], or otherwise); a
             [@staticmethod] has no receiver. *)
          (match lang with
           | Lang.Python -> is_first && not is_static
           | _ -> false))

let get_arity params info lang =
  List.length
    (List.filteri
       (fun i (gp : G.parameter) ->
          not (is_implicit_receiver lang ~is_first:(i =*= 0)
                 ~is_static:info.is_static info.class_name_str gp))
       params)

(* Drop implicit-receiver IL params; G and IL param lists share length/order. *)
let filter_implicit_receiver_params (lang : Lang.t) ~(is_static : bool)
    (class_name_str : string option)
    (g_params : G.parameter list) (il_params : IL.param list)
    : IL.param list =
  match class_name_str with
  | None -> il_params
  | Some _ ->
    List.combine g_params il_params
    |> List.filteri
         (fun i ((gp, ip) : G.parameter * IL.param) ->
            match ip with
            (* Keep IL.ParamReceiver — extractor maps it to BThis without consuming an arg index. *)
            | IL.ParamReceiver _ -> true
            | _ ->
              not (is_implicit_receiver lang ~is_first:(i =*= 0) ~is_static
                     class_name_str gp))
    |> List.map snd

(* [fid_filter] skips IL/CFG build for out-of-subgraph fns.  Records get [file_ast]/[taint_inst] = [None]; callers set them when needed. *)
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
    let fdef_il = AST_to_IL.function_definition lang fdef in
    let cfg = CFG_build.cfg_of_fdef fdef_il in
    { name; class_name_str; method_properties; is_static;
      cfg; fdef; is_lambda_assignment;
      file_ast = None; taint_inst = None }
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

let extract_and_check
    ?(builtin_signature_db : Shape_and_sig.builtin_signature_database option)
    ?(call_graph : Call_graph.G.t option)
    ?(glob_env : Lval_env.t option)
    ~(lang : Lang.t)
    ~(db : Shape_and_sig.signature_database)
    ~(match_on : [ `Sink | `Source ])
    ~(taint_inst : Taint_rule_inst.t)
    ~(ast : G.program)
    ~(detect_findings : bool)
    (info : fun_info)
    : Shape_and_sig.signature_database * PM.t list =
  let updated_db =
    let params = Tok.unbracket info.fdef.G.fparams in
    let arity = get_arity params info lang in
    let sig_cfg =
      let filtered_params =
        filter_implicit_receiver_params lang ~is_static:info.is_static
          info.class_name_str params info.cfg.IL.params
      in
      { info.cfg with IL.params = filtered_params }
    in
    let updated_db, _signature =
      Taint_signature_extractor.extract_signature_with_file_context
        ~arity:(Shape_and_sig.Arity_exact arity) ~db ?builtin_signature_db
        taint_inst ~name:info.name
        ~method_properties:info.method_properties
        ~call_graph:call_graph
        sig_cfg ast
    in
    (* Kotlin trailing-lambda syntax f(a){b}: also extract at arity-1. *)
    if Lang.equal lang Lang.Kotlin && arity >= 1 then
      let last_param_is_lambda =
        match List.rev params with
        | G.Param { G.ptype = Some { t = G.TyFun _; _ }; _ } :: _ ->
            true
        | _ -> false
      in
      if last_param_is_lambda then
        let db', _ =
          Taint_signature_extractor.extract_signature_with_file_context
            ~arity:(Shape_and_sig.Arity_exact (arity - 1)) ~db:updated_db ?builtin_signature_db
            taint_inst ~name:info.name
            ~method_properties:info.method_properties
            ~call_graph:call_graph
            sig_cfg ast
        in
        db'
      else updated_db
    else updated_db
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
      check_fundef_with_cfg taint_inst info.name
        ?glob_env ?class_name:info.class_name_str
        ~signature_db:updated_db ?builtin_signature_db
        ?call_graph
        info.cfg
    in
    let effects_to_record =
    if info.is_lambda_assignment then
      Effects.filter_map keep_src_toSink_only fdef_effects
    else fdef_effects
  in
    let findings = pms_of_effects ~lang ~match_on effects_to_record in
    (updated_db, findings)

(* Class-body initialisers/static blocks aren't call-graph functions.  CFG build is lang+AST only, so it's split out for multi-rule reuse. *)
let build_class_init_cfgs (lang : Lang.t) (ast : G.program)
    : (IL.name option * IL.fun_cfg) list =
  let acc = ref [] in
  Visit_class_defs.visit
    (fun (opt_ent : G.entity option)
      (cdef : G.class_definition) ->
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
      acc := (opt_name, IL.{ params = []; cfg; lambdas }) :: !acc)
    ast;
  !acc

let check_class_inits_prebuilt
    (taint_inst : Taint_rule_inst.t)
    (cfgs : (IL.name option * IL.fun_cfg) list)
    ?(signature_db : Shape_and_sig.signature_database option)
    ?(builtin_signature_db : Shape_and_sig.builtin_signature_database option)
    ?(call_graph : Call_graph.G.t option)
    () : Shape_and_sig.Effects.t =
  List.fold_left
    (fun acc (opt_name, fun_cfg) ->
      let init_effects, _mapping =
        Dataflow_tainting.fixpoint taint_inst ?name:opt_name
          ?signature_db ?builtin_signature_db
          ?call_graph
          fun_cfg
      in
      Shape_and_sig.Effects.union init_effects acc)
    Shape_and_sig.Effects.empty cfgs

let check_class_inits
    (taint_inst : Taint_rule_inst.t)
    (ast : G.program)
    ?(signature_db : Shape_and_sig.signature_database option)
    ?(builtin_signature_db : Shape_and_sig.builtin_signature_database option)
    ?(call_graph : Call_graph.G.t option)
    () : Shape_and_sig.Effects.t =
  check_class_inits_prebuilt taint_inst
    (build_class_init_cfgs taint_inst.lang ast)
    ?signature_db ?builtin_signature_db ?call_graph ()

(* Check the top-level statements.
 * In scripting languages it is not unusual to write code outside
 * function declarations and we want to check this too. We simply
 * treat the program itself as an anonymous function. *)
let build_top_level_cfg (lang : Lang.t) (ast : G.program)
    : IL.name * IL.fun_cfg =
  let xs = AST_to_IL.stmt lang (G.stmt1 ast) in
  let cfg, lambdas = CFG_build.cfg_of_stmts xs in
  (Graph_from_AST.top_level_name_of_ast ast, IL.{ params = []; cfg; lambdas })

let check_top_level_prebuilt
    (taint_inst : Taint_rule_inst.t)
    ((top_level_name, fun_cfg) : IL.name * IL.fun_cfg)
    ?(signature_db : Shape_and_sig.signature_database option)
    ?(builtin_signature_db : Shape_and_sig.builtin_signature_database option)
    ?(call_graph : Call_graph.G.t option)
    () : Shape_and_sig.Effects.t =
  let top_effects, _mapping =
    Dataflow_tainting.fixpoint taint_inst ~name:top_level_name
      ?signature_db ?builtin_signature_db
      ?call_graph
      fun_cfg
  in
  top_effects

let check_top_level
    (taint_inst : Taint_rule_inst.t)
    (ast : G.program)
    ?(signature_db : Shape_and_sig.signature_database option)
    ?(builtin_signature_db : Shape_and_sig.builtin_signature_database option)
    ?(call_graph : Call_graph.G.t option)
    () : Shape_and_sig.Effects.t =
  check_top_level_prebuilt taint_inst
    (build_top_level_cfg taint_inst.lang ast)
    ?signature_db ?builtin_signature_db ?call_graph ()

let check_rule per_file_formula_cache (rule : R.taint_rule) match_hook
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
  (* the matching time spans the taint spec, the per-function, class
   * initialisation and top-level fixpoints, up to the report *)
  let match_start = Core_profiling.now () in
  (* TODO: 'debug_taint' should just be part of 'res'
   * (i.e., add a "debugging" field to 'Report.match_result'). *)
  match
    Match_taint_spec.taint_config_of_rule ~per_file_formula_cache
      xconf lang file (ast, []) rule
  with
  | None -> (None, None)
  | Some (taint_inst, spec_matches, expls) ->
      (* Must match the root used to absolutify the graph/fids below, else dataflow's [Tok.abs_tok] tokens stay relative and miss the graph. *)
      let taint_inst =
        { taint_inst with Taint_rule_inst.project_root = xtarget.project_root }
      in
      let glob_env, glob_effects = Taint_input_env.mk_file_env taint_inst ast in
      let glob_matches = pms_of_effects ~lang ~match_on glob_effects in

      let final_signature_db, relevant_graph, branch_matches =
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
            spec_matches.Match_taint_spec.sources
            |> List.map (fun (rwm, _src) -> rwm.Range_with_metavars.r)
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
          let relevant_graph =
            Graph_reachability.compute_relevant_subgraph call_graph
              ~sources:source_functions ~sinks:sink_functions
          in

          let analysis_order =
            Call_graph.Topo.fold
              (fun fn acc -> fn :: acc)
              relevant_graph []
            |> List.rev
          in
          Log.debug (fun m ->
              m "TAINT_TOPO: Analysis order has %d functions"
                (List.length analysis_order));
          List.iteri
            (fun i node ->
              Log.debug (fun m ->
                  m "TAINT_TOPO: [%d] %s" i (Function_id.show node)))
            analysis_order;

          let process_fun_info info (db, ms) =
            let updated_db, findings =
              extract_and_check
                ?builtin_signature_db
                ~call_graph:relevant_graph
                ~glob_env
                ~lang ~db ~match_on ~taint_inst
                ~ast ~detect_findings:true
                info
            in
            if not (List_.null findings) then
              Log.debug (fun m ->
                  m "FINDING: rule=%s target=%s fn=%s"
                    (Rule_ID.to_string (fst rule.R.id))
                    (Fpath.to_string file)
                    (IL.show_name info.name));
            (updated_db, List.rev_append findings ms)
          in

          let signature_db_after_order, topo_matches =
            List.fold_left
              (fun acc node ->
                Log.debug (fun m ->
                    m "TAINT_SIGBUILD: Processing %s" (Function_id.show_debug node));
                match Shape_and_sig.FunctionMap.find_opt node info_map with
                | None ->
                    Log.debug (fun m ->
                        m "TAINT_SIGBUILD: fn_id NOT FOUND in info_map!");
                    acc
                | Some info ->
                    let (new_db, _) as acc = process_fun_info info acc in
                    Log.debug (fun m ->
                        m
                          "TAINT_SIGBUILD: After processing, db.signatures \
                           size=%d"
                          (Shape_and_sig.FunctionMap.cardinal
                             new_db.Shape_and_sig.signatures));
                    acc)
              (initial_signature_db, []) analysis_order
          in

          (* Skip the "remaining functions" phase entirely - if a function isn't
             in the relevant subgraph, we don't need to analyze it *)
          (Some signature_db_after_order, Some relevant_graph, topo_matches))
        else (
          (* Cross-function taint analysis disabled: use main branch behavior *)
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
                      let _flow, fdef_effects, _mapping =
                        check_fundef taint_inst name ~glob_env
                          ?builtin_signature_db fdef
                      in
                      fdef_matches :=
                        List.rev_append
                          (pms_of_effects ~lang ~match_on fdef_effects)
                          !fdef_matches)
            ast;
          (None, None, !fdef_matches))
      in

      let class_init_effects =
        check_class_inits taint_inst ast
          ?signature_db:final_signature_db ?builtin_signature_db
          ?call_graph:relevant_graph ()
      in
      let class_init_matches =
        pms_of_effects ~lang ~match_on class_init_effects
      in

      let top_matches =
        let top_effects =
          check_top_level taint_inst ast
            ?signature_db:final_signature_db ?builtin_signature_db
            ?call_graph:relevant_graph ()
        in
        pms_of_effects ~lang ~match_on top_effects
      in
      let matches =
        List.concat
          [ glob_matches; branch_matches; class_init_matches; top_matches ]
        (* same post-processing as for search-mode in Match_rules.ml *)
        |> PM.uniq
        |> PM.no_submatches (* see "Taint-tracking via ranges" *) |> match_hook
      in
      let match_time = Core_profiling.since match_start in

      let errors = Parse_target.errors_from_skipped_tokens skipped_tokens in
      let report =
        RP.mk_match_result matches errors
          {
            Core_profiling.rule_id = fst rule.R.id;
            rule_parse_time = parse_time;
            rule_match_time = match_time;
          }
      in
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
  (Dataflow_tainting.reset_constructor ();
   (* Clear the per-domain guard-cond intern table: rules run on a domain in
    * sequence, so a previous rule's canonical conds must not leak into this
    * one. *)
   Effect_guard.reset_intern ();
   match rules with
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

  (* Collect all languages that have rules with taint_intrafile enabled *)
  let langs_needing_call_graph =
    rules
    |> List.fold_left
         (fun acc rule ->
           let xconf_rule =
             Match_env.adjust_xconfig_with_rule_options xconf rule.R.options
           in
           if xconf_rule.config.taint_intrafile then
             match Xlang.to_lang rule.R.target_analyzer with
             | Ok lang -> LangSet.add lang acc
             | Error _ -> acc
           else acc)
         LangSet.empty
  in

  (* Pre-compute call graph and builtin db for each language that needs it.
     The call graph depends on the AST structure and language, so we compute
     it once per language and share across rules that need it. *)
  let ast_call_graph_by_lang =
    LangSet.fold
      (fun lang acc ->
        let ast, _skipped_tokens = lazy_force xtarget.lazy_ast_and_errors in
        Object_initialization.(
          stamp_id_types (detect_object_initialization ast lang) ast);
        let call_graph =
          Graph_from_AST.build_call_graph ~lang ast
        in
        (* Absolutify to match abs_call_tok tokens in Dataflow_tainting. *)
        let call_graph =
          match xtarget.project_root with
          | Some root -> Call_graph.make_paths_absolute root call_graph
          | None -> call_graph
        in
        LangMap.add lang call_graph acc)
      langs_needing_call_graph LangMap.empty
  in

  let builtin_db_by_lang =
    LangSet.fold
      (fun lang acc ->
        let builtin_db = Builtin_models.create_all_builtin_models lang in
        LangMap.add lang builtin_db acc)
      langs_needing_call_graph LangMap.empty
  in

  let results =
    rules
    |> List.filter_map (fun rule ->
           let xconf =
             Match_env.adjust_xconfig_with_rule_options xconf rule.R.options
           in
           (* Only pass call graph and builtin db if taint_intrafile is enabled for this rule *)
           let rule_local_ast_call_graph, rule_builtin_signature_db =
             if xconf.config.taint_intrafile then
               match Xlang.to_lang rule.R.target_analyzer with
               | Ok lang ->
                   ( LangMap.find_opt lang ast_call_graph_by_lang,
                     LangMap.find_opt lang builtin_db_by_lang )
               | Error _ -> (None, None)
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
                       ?builtin_signature_db:rule_builtin_signature_db
                       ~local_ast_call_graph:rule_local_ast_call_graph
                       xconf xtarget
                   in
                   report)))
  in

  results
