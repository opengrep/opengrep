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
module Shape = Shape_and_sig.Shape
module CanonicalBindingMap = Map.Make (struct
  type t = string list

  let compare = Stdlib.compare
end)

type fun_info = {
  name : IL.name;
  class_name_str : string option;
  method_properties : AST_generic.expr list;
  cfg : IL.fun_cfg;
  fdef : G.function_definition;
  is_lambda_assignment : bool;
}

type interfile_rule_context = {
  signature_db : Shape_and_sig.signature_database;
  builtin_signature_db : Shape_and_sig.builtin_signature_database;
  call_graph : Call_graph.G.t;
  imported_global_index : Shape.cell list CanonicalBindingMap.t;
}

type interfile_context = interfile_rule_context Rule_ID.Map.t

type project_target = {
  xtarget : Xtarget.t;
  ast : G.program;
  taint_inst : Taint_rule_inst.t;
  spec_matches : Match_taint_spec.spec_matches;
  ctx : AST_to_IL.ctx;
  object_mappings : (G.name * G.name) list;
  info_map : fun_info Shape_and_sig.FunctionMap.t;
}

type project_fun_info = {
  target : project_target;
  info : fun_info;
}

let add_imported_global_binding imported_global_index canonical cell =
  CanonicalBindingMap.update canonical
    (function
      | Some cells when List.exists (Shape.equal_cell cell) cells -> Some cells
      | Some cells -> Some (cell :: cells)
      | None -> Some [ cell ])
    imported_global_index

let rec starts_with_segments ~prefix segments =
  match (prefix, segments) with
  | [], _ -> true
  | _, [] -> false
  | p :: prefix_rest, s :: segments_rest ->
      String.equal p s && starts_with_segments ~prefix:prefix_rest segments_rest

let lookup_imported_global_binding ?current_file imported_global_index canonical
    =
  Graph_from_AST.canonical_lookup_candidates ?current_file canonical
  |> List.find_map (fun candidate ->
         match CanonicalBindingMap.find_opt candidate imported_global_index with
         | Some [ cell ] -> Some cell
         | Some _
         | None ->
             None)

let add_tainted_bindings_for_file imported_global_index ~(file : Fpath.t)
    (env : Lval_env.env) =
  let module_candidates = Graph_from_AST.module_candidates_of_path file in
  Lval_env.seq_of_tainted env
  |> Seq.fold_left
       (fun acc (name, cell) ->
         if Tok.is_fake (snd name.IL.ident) then acc
         else
           module_candidates
           |> List.fold_left
                (fun acc module_name ->
                  add_imported_global_binding acc
                    (module_name @ [ fst name.IL.ident ])
                    cell)
                acc)
       imported_global_index

let add_top_level_imported_global_bindings lookup_index
    (imported_global_index : Shape.cell list CanonicalBindingMap.t)
    ~(file : Fpath.t) (ast : G.program) =
  let module_candidates = Graph_from_AST.module_candidates_of_path file in
  let add_module_binding acc local_name cell =
    module_candidates
    |> List.fold_left
         (fun acc module_name ->
           add_imported_global_binding acc (module_name @ [ local_name ]) cell)
         acc
  in
  ast
  |> List.fold_left
       (fun acc stmt ->
         match stmt.G.s with
         | G.DirectiveStmt directive -> (
             match directive.G.d with
             | G.ImportFrom (_, G.DottedName xs, imported_names) ->
                 imported_names
                 |> List.fold_left
                      (fun acc (id, alias_opt) ->
                        let local_name =
                          match alias_opt with
                          | Some (alias, _id_info) -> fst alias
                          | None -> fst id
                        in
                        let canonical = G.dotted_to_canonical (xs @ [ id ]) in
                        match
                          lookup_imported_global_binding ~current_file:file
                            lookup_index canonical
                        with
                        | Some cell -> add_module_binding acc local_name cell
                        | None -> acc)
                      acc
             | G.ImportAll (_, G.DottedName xs, _) ->
                 let resolved_targets =
                   Graph_from_AST.canonical_lookup_candidates
                     ~current_file:file
                     (G.dotted_to_canonical xs)
                 in
                 CanonicalBindingMap.fold
                   (fun canonical cells acc ->
                     match cells with
                     | [ cell ] -> (
                         match
                           List.find_opt
                             (fun target ->
                               starts_with_segments ~prefix:target canonical)
                             resolved_targets
                         with
                         | Some target -> (
                             match
                               List_.drop (List.length target) canonical
                             with
                             | [ local_name ] ->
                                 add_module_binding acc local_name cell
                             | _ -> acc)
                         | None -> acc)
                     | _ -> acc)
                   lookup_index acc
             | _ -> acc)
         | _ -> acc)
       imported_global_index

let imported_global_index_equal left right =
  CanonicalBindingMap.equal (List.equal Shape.equal_cell) left right

let build_imported_global_index
    ~(signature_db : Shape_and_sig.signature_database)
    ~(builtin_signature_db : Shape_and_sig.builtin_signature_database)
    ~(call_graph : Call_graph.G.t) (project_targets : project_target list) =
  let base_index =
    project_targets
    |> List.fold_left
         (fun acc (target : project_target) ->
           let file = target.xtarget.path.internal_path_to_content in
           let stmts =
             AST_to_IL.stmt target.taint_inst.lang (G.stmt1 target.ast)
           in
           let cfg, lambdas = CFG_build.cfg_of_stmts stmts in
           let top_level_name =
             Graph_from_AST.top_level_il_name ~current_file:file target.ast
           in
           let _top_effects, end_mapping =
             D.fixpoint target.taint_inst ~name:top_level_name
               ~signature_db ~builtin_signature_db ~call_graph
               IL.{ params = []; cfg; lambdas }
           in
           let exit_env = end_mapping.(cfg.exit).Dataflow_core.out_env in
           add_tainted_bindings_for_file acc ~file exit_env)
         CanonicalBindingMap.empty
  in
  let rec close_over_top_level_imports imported_global_index =
    let next_index =
      project_targets
      |> List.fold_left
           (fun acc (target : project_target) ->
             add_top_level_imported_global_bindings imported_global_index acc
               ~file:target.xtarget.path.internal_path_to_content target.ast)
           imported_global_index
    in
    if imported_global_index_equal imported_global_index next_index then
      imported_global_index
    else close_over_top_level_imports next_index
  in
  close_over_top_level_imports base_index

let imported_global_env_of_ast ~(file : Fpath.t)
    (imported_global_index : Shape.cell list CanonicalBindingMap.t)
    (ast : G.program) =
  let env = ref Lval_env.empty in
  let imported_module_bindings =
    let bindings = ref SMap.empty in
    let visitor =
      object
        inherit [_] G.iter as super

        method! visit_directive env directive =
          (match directive.G.d with
          | G.ImportAs (_, G.DottedName (head :: _ as xs), alias_opt) ->
              let local_name =
                match alias_opt with
                | Some (alias, _id_info) -> fst alias
                | None -> fst head
              in
              bindings := SMap.add local_name (G.dotted_to_canonical xs) !bindings
          | _ -> ());
          super#visit_directive env directive
      end
    in
    visitor#visit_program () ast;
    !bindings
  in
  let add_imported_global_path (base_id, base_id_info) field_path canonical =
    match
      lookup_imported_global_binding ~current_file:file imported_global_index
        canonical
    with
    | None -> ()
    | Some (Shape.Cell (xtaint, shape)) ->
        let base_name = AST_to_IL.var_of_id_info base_id base_id_info in
        let offset =
          field_path
          |> List_.map (fun (field_id, field_id_info) ->
                 T.Ofld (AST_to_IL.var_of_id_info field_id field_id_info))
        in
        env :=
          Lval_env.add_shape base_name offset (Xtaint.to_taints xtaint) shape
            !env
  in
  let add_imported_global id id_info canonical =
    add_imported_global_path (id, id_info) [] canonical
  in
  let rec dot_access_path (expr : G.expr) =
    match expr.G.e with
    | G.N (G.Id (id, id_info)) -> Some ((id, id_info), [])
    | G.DotAccess (base_expr, _, G.FN (G.Id (field_id, field_id_info))) -> (
        match dot_access_path base_expr with
        | None -> None
        | Some (base, field_path) ->
            Some (base, field_path @ [ (field_id, field_id_info) ]))
    | _ -> None
  in
  let visitor =
    object
      inherit [_] G.iter_no_id_info as super

      method! visit_expr env_ expr =
        (match expr.G.e with
        | G.N (G.Id (id, id_info)) -> (
            match !(id_info.G.id_resolved) with
            | Some (G.ImportedEntity canonical, _sid) ->
                add_imported_global id id_info canonical
            | Some _ ->
                ()
            | None ->
                add_imported_global id id_info [ fst id ])
        | G.N
            (G.IdQualified
              { name_last = (id, _typeargsTODO); name_info; _ }) -> (
            match !(name_info.G.id_resolved) with
            | Some (G.ImportedEntity canonical, _sid) ->
                add_imported_global id name_info canonical
            | Some _ ->
                ()
            | None ->
                ())
        | G.DotAccess (_, _, G.FN (G.Id _)) -> (
            match dot_access_path expr with
            | Some ((base_id, base_id_info), ((_field_id, _) :: _ as field_path))
              -> (
                let canonical_module_opt =
                  match !(base_id_info.G.id_resolved) with
                  | Some (G.ImportedModule canonical, _sid)
                  | Some (G.ImportedEntity canonical, _sid) ->
                      Some canonical
                  | Some _
                  | None ->
                      SMap.find_opt (fst base_id) imported_module_bindings
                in
                match canonical_module_opt with
                | Some canonical ->
                    let canonical =
                      canonical @ (field_path |> List_.map (fun (id, _) -> fst id))
                    in
                    add_imported_global_path (base_id, base_id_info) field_path
                      canonical
                | None ->
                    ())
            | Some _
            | None ->
                ())
        | _ -> ());
        super#visit_expr env_ expr
    end
  in
  visitor#visit_program () ast;
  !env

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Wrapper around the tainting dataflow-based analysis. *)

(*****************************************************************************)
(* Hooks *)
(*****************************************************************************)

let hook_setup_hook_function_taint_signature = ref None

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
    |> List_.filter_map (fun { Effect.taint = { orig; tokens }; sink_trace } ->
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

let pms_of_effect ~match_on (effect_ : Effect.t) =
  match effect_ with
  | ToLval _
  | ToReturn _
  | ToSinkInCall _ ->
      []
  | ToSink
      {
        taints_with_precondition = taints, requires;
        sink = { pm = sink_pm; _ } as sink;
        merged_env;
      } -> (
      let actual_taints = List_.map (fun t -> t.Effect.taint) taints in
      let satisfies = T.taints_satisfy_requires actual_taints requires in
      if not satisfies then []
      else (
        let preferred_label = preferred_label_of_sink sink in
        match sources_of_taints ?preferred_label taints with
        | [] -> []
        | taint_sources -> (
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
        )
      )

(*****************************************************************************)
(* Main entry points *)
(*****************************************************************************)

let check_fundef (taint_inst : Taint_rule_inst.t) (name : IL.name) ctx ?glob_env ?class_name
    ?signature_db ?builtin_signature_db ?call_graph fdef =
  let fdef = AST_to_IL.function_definition taint_inst.lang ~ctx fdef in
  let fcfg = CFG_build.cfg_of_fdef fdef in
  let in_env, env_effects =
    Taint_input_env.mk_fun_input_env taint_inst ?glob_env fdef.fparams
  in
  let effects, mapping =
    Dataflow_tainting.fixpoint taint_inst ~in_env ~name ?class_name
      ?signature_db ?builtin_signature_db ?call_graph fcfg
  in
  let effects = Effects.union env_effects effects in
  (fcfg, effects, mapping)

let get_arity params info lang =
  let filtered_params =
    match (lang, info.class_name_str) with
    (* Python methods: filter out 'self' and 'cls' params *)
    | Lang.Python, Some _ ->
        List.filter
          (function
            | G.Param { pname = Some (("self" | "cls"), _); _ } -> false
            | _ -> true)
          params
    (* Go methods: filter out ParamReceiver *)
    | Lang.Go, Some _ ->
        List.filter
          (function
            | G.ParamReceiver _ -> false
            | _ -> true)
          params
    | _ -> params
  in
  List.length filtered_params

let filtered_signature_params params info lang =
  match (lang, info.class_name_str) with
  (* Python methods: filter out 'self' and 'cls' params *)
  | Lang.Python, Some _ ->
      List.filter
        (function
          | G.Param { pname = Some (("self" | "cls"), _); _ } -> false
          | _ -> true)
        params
  (* Go methods: filter out ParamReceiver *)
  | Lang.Go, Some _ ->
      List.filter
        (function
          | G.ParamReceiver _ -> false
          | _ -> true)
        params
  | _ -> params

let rec trailing_default_params acc = function
  | G.Param { pname = Some id; pinfo = id_info; pdefault = Some default; _ }
    :: rest ->
      trailing_default_params ((id, id_info, default) :: acc) rest
  | [] -> acc
  | _ :: _ -> acc

let python_trailing_default_params params info =
  filtered_signature_params params info Lang.Python |> List.rev
  |> trailing_default_params [] |> List.rev

let default_assignment_stmt (id, id_info, default_expr) =
  G.Assign (G.N (G.Id (id, id_info)) |> G.e, Tok.fake_tok (snd id) "=", default_expr)
  |> G.e |> G.exprstmt

let prepend_default_assignments defaults (fbody : G.function_body) :
    G.function_body =
  let body_stmt = H.funcbody_to_stmt fbody in
  let prologue = defaults |> List_.map default_assignment_stmt in
  G.FBStmt (G.Block (Tok.unsafe_fake_bracket (prologue @ [ body_stmt ])) |> G.s)

let extract_single_arity_signatures ~(lang : Lang.t) ~(ctx : AST_to_IL.ctx)
    ~(taint_inst : Taint_rule_inst.t) ~(ast : G.program)
    ?(builtin_signature_db : Shape_and_sig.builtin_signature_database option)
    ~(call_graph : Call_graph.G.t) (info : fun_info)
    (db : Shape_and_sig.signature_database) :
    Shape_and_sig.signature_database =
  let params = Tok.unbracket info.fdef.fparams in
  let arity = get_arity params info lang in
  let extract_signature_for cfg arity db =
    fst
      (Taint_signature_extractor.extract_signature_with_file_context
         ~arity:(Shape_and_sig.Arity_exact arity) ~db ?builtin_signature_db
         taint_inst ~name:info.name
         ~method_properties:info.method_properties
         ~call_graph:(Some call_graph) cfg ast)
  in
  let updated_db =
    extract_signature_for info.cfg arity db
  in
  let updated_db =
    if Lang.equal lang Lang.Python then
      let default_suffix = python_trailing_default_params params info in
      default_suffix
      |> List_.mapi (fun i _ -> i + 1)
      |> List.fold_left
           (fun acc omitted_count ->
             let kept_params =
               List_.take (List.length params - omitted_count) params
             in
             let kept_arity = arity - omitted_count in
             let omitted_defaults =
               default_suffix
               |> List_.drop (List.length default_suffix - omitted_count)
             in
             let synthetic_fdef : G.function_definition =
               {
                 info.fdef with
                 G.fparams = Tok.unsafe_fake_bracket kept_params;
                 fbody = prepend_default_assignments omitted_defaults info.fdef.fbody;
               }
             in
             let fdef_il =
               AST_to_IL.function_definition lang ~ctx synthetic_fdef
             in
             let cfg = CFG_build.cfg_of_fdef fdef_il in
             extract_signature_for cfg kept_arity acc)
           updated_db
    else updated_db
  in
  if Lang.equal lang Lang.Kotlin && arity >= 1 then
    let last_param_is_lambda =
      match List.rev params with
      | G.Param { G.ptype = Some { t = G.TyFun _; _ }; _ } :: _ -> true
      | _ -> false
    in
    if last_param_is_lambda then
      fst
        (Taint_signature_extractor.extract_signature_with_file_context
           ~arity:(Shape_and_sig.Arity_exact (arity - 1))
           ~db:updated_db ?builtin_signature_db taint_inst ~name:info.name
           ~method_properties:info.method_properties
           ~call_graph:(Some call_graph) info.cfg ast)
    else updated_db
  else updated_db

(** Convert a Case pattern back into a [G.parameter list] for per-arity
    signature extraction (Clojure multi-arity / Elixir multi-clause). *)
let params_of_case_pattern (pat : G.pattern) : G.parameter list =
  let unwrap_guard (p : G.pattern) : G.pattern =
    match p with
    | G.PatWhen (inner, _guard) -> inner
    | _ -> p
  in
  let param_of_pat (p : G.pattern) : G.parameter =
    match p with
    | G.PatId (ident, id_info) ->
        G.Param
          {
            G.pname = Some ident;
            pinfo = id_info;
            ptype = None;
            pdefault = None;
            pattrs = [];
          }
    | G.PatConstructor (G.Id (("&", _amp_tok), _), [ G.PatId (ident, id_info) ])
      ->
        (* Clojure rest params: (& rest) *)
        G.ParamRest
          ( Tok.unsafe_fake_tok "&",
            {
              G.pname = Some ident;
              pinfo = id_info;
              ptype = None;
              pdefault = None;
              pattrs = [];
            } )
    | _ -> G.OtherParam (("PatUnknown", G.fake ""), [])
  in
  let inner = unwrap_guard pat in
  let pats =
    match inner with
    | G.PatList (_, pats, _)
    | G.PatTuple (_, pats, _) ->
        pats
    | _ -> [ inner ]
  in
  List_.map param_of_pat pats

(** For Clojure/Elixir functions with a single implicit param and a Switch
    body, extract per-arity cases. Returns a list of
    (params, function_body, sig_arity) sorted by decreasing arity. *)
let extract_multi_arity_cases (fdef : G.function_definition) :
    (G.parameter list * G.function_body * Shape_and_sig.sig_arity) list option =
  let params = Tok.unbracket fdef.G.fparams in
  let has_implicit =
    match params with
    | [ G.Param { G.pname = Some (name, _); _ } ] ->
        G.is_implicit_param name
    | _ -> false
  in
  if not has_implicit then None
  else
    match fdef.G.fbody with
    | G.FBStmt { G.s = G.Switch (_, _, cases); _ } ->
        let arity_cases =
          cases
          |> List.filter_map (fun (cab : G.case_and_body) ->
                 match cab with
                 | G.CasesAndBody (case_list, body) -> (
                     match case_list with
                     | [ G.Case (_, pat) ] ->
                         let case_params = params_of_case_pattern pat in
                         let rest, fixed =
                           List.partition
                             (function
                               | G.ParamRest _ -> true
                               | _ -> false)
                             case_params
                         in
                         let arity : Shape_and_sig.sig_arity =
                           match rest with
                           | _ :: _ -> Arity_at_least (List.length fixed)
                           | [] -> Arity_exact (List.length fixed)
                         in
                         Some (case_params, G.FBStmt body, arity)
                     | _ -> None)
                 | G.CaseEllipsis _ -> None)
        in
        let sorted =
          List.sort
            (fun (_, _, a1) (_, _, a2) ->
              let n1 = Shape_and_sig.int_of_sig_arity a1 in
              let n2 = Shape_and_sig.int_of_sig_arity a2 in
              Int.compare n2 n1)
            arity_cases
        in
        (match sorted with
        | [] -> None
        | _ -> Some sorted)
    | _ -> None

let build_ast_ctx (ast : G.program) : AST_to_IL.ctx =
  let ctx = ref AST_to_IL.empty_ctx in
  Visit_function_defs.visit
    (fun opt_ent _fdef ->
      match opt_ent with
      | Some { name = EN (Id (n, _)); _ } ->
          ctx := AST_to_IL.add_entity_name !ctx n
      | __else__ -> ())
    ast;
  !ctx

let collect_fun_info_map ~(lang : Lang.t) ~(ctx : AST_to_IL.ctx)
    (ast : G.program) : fun_info Shape_and_sig.FunctionMap.t =
  let add_info info info_map =
    let fn_id = Function_id.of_il_name info.name in
    if Shape_and_sig.FunctionMap.mem fn_id info_map then info_map
    else Shape_and_sig.FunctionMap.add fn_id info info_map
  in
  Visit_function_defs.fold_with_parent_path
    (fun info_map opt_ent parent_path fdef ->
      match fst fdef.fkind with
      | LambdaKind
      | Arrow -> (
          match opt_ent with
          | None -> info_map
          | Some ent -> (
              match AST_to_IL.name_of_entity ent with
              | None -> info_map
          | Some name ->
              let class_name_str =
                match parent_path with
                | Some class_il :: _ -> Some (fst class_il.IL.ident)
                | _ -> None
              in
              let fdef_il =
                AST_to_IL.function_definition lang ~ctx fdef
              in
              let cfg = CFG_build.cfg_of_fdef fdef_il in
              let info =
                {
                  name;
                  class_name_str;
                  method_properties = [];
                  cfg;
                  fdef;
                  is_lambda_assignment = true;
                }
              in
              add_info info info_map))
      | Function
      | Method
      | BlockCases -> (
          match Option.bind opt_ent AST_to_IL.name_of_entity with
          | None -> info_map
          | Some name ->
              let go_receiver_name =
                match lang with
                | Lang.Go -> Graph_from_AST.extract_go_receiver_type fdef
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
              let method_properties =
                match fst fdef.fkind with
                | Method ->
                    Taint_signature_extractor.extract_method_properties fdef
                | Function
                | LambdaKind
                | Arrow
                | BlockCases ->
                    []
              in
              let fdef_il =
                AST_to_IL.function_definition lang ~ctx fdef
              in
              let cfg = CFG_build.cfg_of_fdef fdef_il in
              let info =
                {
                  name;
                  class_name_str;
                  method_properties;
                  cfg;
                  fdef;
                  is_lambda_assignment = false;
                }
              in
              add_info info info_map))
    Shape_and_sig.FunctionMap.empty ast

let add_signatures_for_fun_info ~(lang : Lang.t) ~(ctx : AST_to_IL.ctx)
    ~(taint_inst : Taint_rule_inst.t) ~(ast : G.program)
    ?(builtin_signature_db : Shape_and_sig.builtin_signature_database option)
    ~(call_graph : Call_graph.G.t) (info : fun_info)
    (db : Shape_and_sig.signature_database) :
    Shape_and_sig.signature_database =
  match extract_multi_arity_cases info.fdef with
  | Some arity_cases ->
      List.fold_left
        (fun acc_db (case_params, case_body, arity) ->
          let synthetic_fdef : G.function_definition =
            {
              G.fparams = Tok.unsafe_fake_bracket case_params;
              frettype = None;
              fkind = info.fdef.G.fkind;
              fbody = case_body;
            }
          in
          let fdef_il =
            AST_to_IL.function_definition lang ~ctx synthetic_fdef
          in
          let cfg = CFG_build.cfg_of_fdef fdef_il in
          let db', _sig =
            Taint_signature_extractor.extract_signature_with_file_context
              ~arity ~db:acc_db ?builtin_signature_db taint_inst
              ~name:info.name
              ~method_properties:info.method_properties
              ~call_graph:(Some call_graph) cfg ast
          in
          db')
        db arity_cases
  | None ->
      extract_single_arity_signatures ~lang ~ctx ~taint_inst ~ast
        ?builtin_signature_db ~call_graph info db

let check_function_defs_for_matches ~(lang : Lang.t) ~(ctx : AST_to_IL.ctx)
    ~(taint_inst : Taint_rule_inst.t) ~(glob_env : Taint_lval_env.t)
    ?(signature_db : Shape_and_sig.signature_database option)
    ?(builtin_signature_db : Shape_and_sig.builtin_signature_database option)
    ?(call_graph : Call_graph.G.t option)
    ~(record_matches : Shape_and_sig.Effects.t -> unit) (ast : G.program) : unit =
  let info_map = collect_fun_info_map ~lang ~ctx ast in
  Shape_and_sig.FunctionMap.iter
    (fun _fn_id info ->
      if not info.is_lambda_assignment then (
        Log.info (fun m ->
            m
              "Match_tainting_mode:\n\
               --------------------\n\
               Checking func def: %s\n\
               --------------------"
              (IL.str_of_name info.name));
        let _flow, fdef_effects, _mapping =
          check_fundef taint_inst info.name ctx ~glob_env
            ?class_name:info.class_name_str ?signature_db
            ?builtin_signature_db ?call_graph info.fdef
        in
        record_matches fdef_effects))
    info_map

let build_interfile_rule_context (xconf : Match_env.xconfig)
    (rule : R.taint_rule) (xtargets : Xtarget.t list) :
    interfile_rule_context option =
  match Xlang.to_lang rule.R.target_analyzer with
  | Error _ -> None
  | Ok lang ->
      let builtin_signature_db =
        Builtin_models.create_all_builtin_models lang
      in
      let project_targets =
        xtargets
        |> List_.filter_map (fun (xtarget : Xtarget.t) ->
               let file = xtarget.path.internal_path_to_content in
               let ast, _skipped_tokens = lazy_force xtarget.lazy_ast_and_errors in
               let per_file_formula_cache =
                 Formula_cache.mk_specialized_formula_cache [ rule ]
               in
               let* taint_inst, spec_matches, _expls =
                 Match_taint_spec.taint_config_of_rule
                   ~per_file_formula_cache ~require_source_sink:false xconf
                   lang file (ast, []) rule
               in
               let ctx = build_ast_ctx ast in
               let object_mappings =
                 Taint_signature_extractor.detect_object_initialization ast
                   taint_inst.lang
               in
               let info_map = collect_fun_info_map ~lang ~ctx ast in
               Some
                 {
                   xtarget;
                   ast;
                   taint_inst;
                   spec_matches;
                   ctx;
                   object_mappings;
                   info_map;
                 })
      in
      let project_graph_inputs =
        project_targets
        |> List_.map (fun target ->
               (target.xtarget.path, target.ast, target.object_mappings))
      in
      let call_graph =
        Graph_from_AST.build_project_call_graph ~lang project_graph_inputs
      in
      let source_functions =
        project_targets
        |> List.concat_map (fun target ->
               let source_ranges =
                 target.spec_matches.sources
                 |> List_.map (fun (rwm, _src) -> rwm.Range_with_metavars.r)
               in
               Graph_from_AST.find_functions_containing_ranges ~lang
                 ~current_file:target.xtarget.path.internal_path_to_content
                 target.ast source_ranges)
        |> List.sort_uniq Function_id.compare
      in
      let sink_functions =
        project_targets
        |> List.concat_map (fun target ->
               let sink_ranges =
                 target.spec_matches.sinks
                 |> List_.map (fun (rwm, _sink) -> rwm.Range_with_metavars.r)
               in
               Graph_from_AST.find_functions_containing_ranges ~lang
                 ~current_file:target.xtarget.path.internal_path_to_content
                 target.ast sink_ranges)
        |> List.sort_uniq Function_id.compare
      in
      let relevant_graph =
        Graph_reachability.compute_relevant_subgraph call_graph
          ~sources:source_functions ~sinks:sink_functions
      in
      let analysis_order =
        Call_graph.Topo.fold (fun fn acc -> fn :: acc) relevant_graph []
        |> List.rev
      in
      let base_db =
        Builtin_models.init_signature_database None
      in
      let object_mappings =
        project_targets
        |> List.concat_map (fun target -> target.object_mappings)
      in
      let initial_signature_db =
        Shape_and_sig.add_object_mappings base_db object_mappings
      in
      let project_info_map =
        project_targets
        |> List.fold_left
             (fun acc target ->
               Shape_and_sig.FunctionMap.fold
                 (fun fn_id info acc ->
                   if Shape_and_sig.FunctionMap.mem fn_id acc then acc
                   else
                     Shape_and_sig.FunctionMap.add fn_id { target; info } acc)
                 target.info_map acc)
             Shape_and_sig.FunctionMap.empty
      in
      let signature_db =
        List.fold_left
          (fun db node ->
            match Shape_and_sig.FunctionMap.find_opt node project_info_map with
            | None -> db
            | Some { target; info } ->
                add_signatures_for_fun_info ~lang ~ctx:target.ctx
                  ~taint_inst:target.taint_inst ~ast:target.ast
                  ?builtin_signature_db:(Some builtin_signature_db)
                  ~call_graph:relevant_graph info db)
          initial_signature_db analysis_order
      in
      let imported_global_index =
        build_imported_global_index ~signature_db ~builtin_signature_db
          ~call_graph:relevant_graph project_targets
      in
      Some
        {
          signature_db;
          builtin_signature_db;
          call_graph = relevant_graph;
          imported_global_index;
        }

let build_interfile_contexts (xconf : Match_env.xconfig)
    (rule_targets : (R.taint_rule * Xtarget.t list) list) : interfile_context =
  rule_targets
  |> List.fold_left
       (fun acc (rule, xtargets) ->
         match build_interfile_rule_context xconf rule xtargets with
         | None -> acc
         | Some context -> Rule_ID.Map.add (fst rule.R.id) context acc)
       Rule_ID.Map.empty

let check_rule per_file_formula_cache (rule : R.taint_rule) match_hook
    ?(signature_db : Shape_and_sig.signature_database option)
    ?(builtin_signature_db : Shape_and_sig.builtin_signature_database option)
    ?(interfile_rule_context : interfile_rule_context option)
    ?(shared_call_graph :
        (Call_graph.G.t * (G.name * G.name) list) option =
      None) (xconf : Match_env.xconfig) (xtarget : Xtarget.t) =
  Log.info (fun m ->
      m
        "Match_tainting_mode:\n\
         ====================\n\
         Running rule %s\n\
         ===================="
        (Rule_ID.to_string (fst rule.R.id)));
  let matches = ref [] in
  let match_on =
    (* TEMPORARY HACK to support both taint_match_on (DEPRECATED) and
     * taint_focus_on (preferred name by SR). *)
    match (xconf.config.taint_focus_on, xconf.config.taint_match_on) with
    | `Source, _
    | _, `Source ->
        `Source
    | `Sink, `Sink -> `Sink
  in
  let record_matches new_effects =
    new_effects
    |> Effects.iter (fun effect_ ->
           let effect_pms = pms_of_effect ~match_on effect_ in
           matches := List.rev_append effect_pms !matches)
  in
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
    Common.with_time (fun () -> lazy_force lazy_ast_and_errors)
  in
  (* TODO: 'debug_taint' should just be part of 'res'
   * (i.e., add a "debugging" field to 'Report.match_result'). *)
  match
    Match_taint_spec.taint_config_of_rule ~per_file_formula_cache
      ~require_source_sink:(not xconf.config.interfile) xconf lang file
      (ast, []) rule
  with
  | None -> (None, None)
  | Some (taint_inst, spec_matches, expls) ->
      let ctx = build_ast_ctx ast in

      let file_glob_env, glob_effects = Taint_input_env.mk_file_env taint_inst ast in
      let imported_glob_env =
        match interfile_rule_context with
        | Some context ->
            imported_global_env_of_ast ~file context.imported_global_index ast
        | None -> Lval_env.empty
      in
      let glob_env = Lval_env.union file_glob_env imported_glob_env in
      record_matches glob_effects;
      let builtin_signature_db =
        match interfile_rule_context with
        | Some context -> Some context.builtin_signature_db
        | None -> builtin_signature_db
      in

      (* Only use signature database if cross-function taint analysis is enabled *)
      let final_signature_db, relevant_graph, needs_function_match_pass =
        match interfile_rule_context with
        | Some context ->
            (Some context.signature_db, Some context.call_graph, true)
        | None when taint_inst.options.taint_intrafile -> (
          (* Detect object initialization mappings for this file *)
          let object_mappings =
            Taint_signature_extractor.detect_object_initialization ast
              taint_inst.lang
          in
          (* Build user signature database *)
          let base_db = Builtin_models.init_signature_database signature_db in
          (* Note: object_mappings will be combined with anonymous class mappings
           * and added to the signature database after IL conversion *)

          (* Collect function metadata and prepare call graph based ordering. *)
          let info_map = collect_fun_info_map ~lang ~ctx ast in
          (* Use object mappings from Object_initialization.ml *)
          let all_object_mappings = object_mappings in
          let initial_signature_db =
            Shape_and_sig.add_object_mappings base_db all_object_mappings
          in

          (* Use shared call graph if provided, otherwise compute it *)
          let call_graph =
            match shared_call_graph with
            | Some (graph, _shared_mappings) -> graph
            | None ->
                (* Compute call graph as before *)
                Graph_from_AST.build_call_graph ~lang
                  ~object_mappings:all_object_mappings ast
          in

          (* Optimize: filter call graph to only functions relevant for this rule
             Use the already-computed source/sink ranges from spec_matches *)
          let source_ranges =
            spec_matches.sources
            |> List.map (fun (rwm, _src) -> rwm.Range_with_metavars.r)
          in
          let sink_ranges =
            spec_matches.sinks
            |> List.map (fun (rwm, _sink) -> rwm.Range_with_metavars.r)
          in
          let source_functions =
            Graph_from_AST.find_functions_containing_ranges ~lang ast
              source_ranges
          in
          let sink_functions =
            Graph_from_AST.find_functions_containing_ranges ~lang ast
              sink_ranges
          in
          Log.debug (fun m ->
              m "SUBGRAPH: Found %d source functions and %d sink functions"
                (List.length source_functions)
                (List.length sink_functions));
          List.iteri
            (fun i id ->
              Log.debug (fun m ->
                  let name = Function_id.show id in
                  m "SUBGRAPH: source_function[%d] = %s" i name))
            source_functions;
          List.iteri
            (fun i id ->
              Log.debug (fun m ->
                  let name = Function_id.show id in
                  m "SUBGRAPH: sink_function[%d] = %s" i name))
            sink_functions;

          (* Write FULL call graph to dot file for debugging. Keeping for debugger *)
          (* let full_dot_file = open_out "call_graph_full.dot" in
          Call_graph.Dot.output_graph full_dot_file call_graph;
          close_out full_dot_file;
          Log.debug (fun m -> m "FULL GRAPH: Wrote full call graph to call_graph_full.dot"); *)
          let relevant_graph =
            Graph_reachability.compute_relevant_subgraph call_graph
              ~sources:source_functions ~sinks:sink_functions
          in

          (* Write call graph to dot file for debugging *)
          (* let dot_file = open_out "call_graph.dot" in
          Call_graph.Dot.output_graph dot_file relevant_graph;
          close_out dot_file;
          Log.debug (fun m -> m "SUBGRAPH: Wrote call graph to call_graph.dot"); *)
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

          let run_check_fundef_if_needed (info : fun_info)
              (updated_db : Shape_and_sig.signature_database) :
              Shape_and_sig.signature_database =
            if info.is_lambda_assignment then updated_db
            else begin
              let _flow, fdef_effects, _mapping =
                check_fundef taint_inst info.name ctx ~glob_env
                  ?class_name:info.class_name_str ~signature_db:updated_db
                  ?builtin_signature_db
                  ?call_graph:(Some relevant_graph) info.fdef
              in
              record_matches fdef_effects;
              updated_db
            end
          in

          let process_fun_info info db =
            match extract_multi_arity_cases info.fdef with
            | Some arity_cases ->
                (* Multi-arity function: extract one signature per arity branch *)
                let updated_db =
                  List.fold_left
                    (fun acc_db (case_params, case_body, arity) ->
                      let synthetic_fdef : G.function_definition =
                        {
                          G.fparams = Tok.unsafe_fake_bracket case_params;
                          frettype = None;
                          fkind = info.fdef.G.fkind;
                          fbody = case_body;
                        }
                      in
                      let fdef_il =
                        AST_to_IL.function_definition lang ~ctx
                          synthetic_fdef
                      in
                      let cfg = CFG_build.cfg_of_fdef fdef_il in
                      let db', _sig =
                        Taint_signature_extractor
                        .extract_signature_with_file_context ~arity ~db:acc_db
                          ?builtin_signature_db taint_inst ~name:info.name
                          ~method_properties:info.method_properties
                          ~call_graph:(Some relevant_graph) cfg ast
                      in
                      db')
                    db arity_cases
                in
                run_check_fundef_if_needed info updated_db
            | None ->
                let updated_db =
                  extract_single_arity_signatures ~lang ~ctx ~taint_inst ~ast
                    ?builtin_signature_db ~call_graph:relevant_graph info db
                in
                run_check_fundef_if_needed info updated_db
          in

          let signature_db_after_order =
            List.fold_left
              (fun db node ->
                Log.debug (fun m ->
                    m "TAINT_SIGBUILD: Processing %s" (Function_id.show node));
                match Shape_and_sig.FunctionMap.find_opt node info_map with
                | None ->
                    Log.debug (fun m ->
                        m "TAINT_SIGBUILD: fn_id NOT FOUND in info_map!");
                    db
                | Some info ->
                    Log.debug (fun m ->
                        m
                          "TAINT_SIGBUILD: fn_id found in info_map, \
                           processing...");
                    let new_db = process_fun_info info db in
                    Log.debug (fun m ->
                        m
                          "TAINT_SIGBUILD: After processing, db.signatures \
                           size=%d"
                          (Shape_and_sig.FunctionMap.cardinal
                             new_db.Shape_and_sig.signatures));
                    new_db)
              initial_signature_db analysis_order
          in

          let final_signature_db =
            Shape_and_sig.FunctionMap.fold
              (fun fn_id info db ->
                if
                  Shape_and_sig.FunctionMap.mem fn_id
                    db.Shape_and_sig.signatures
                then db
                else
                  (* Keep the relevance-filtered pass for performance, but add
                     missing signatures from the full graph. Top-level direct
                     calls and callback sources can otherwise be skipped because
                     the call graph is oriented callee -> caller. *)
                  add_signatures_for_fun_info ~lang ~ctx ~taint_inst ~ast
                    ?builtin_signature_db ~call_graph info db)
              info_map signature_db_after_order
          in
          (Some final_signature_db, Some relevant_graph, false))
        | None ->
          (* Cross-function taint analysis disabled: use main branch behavior *)
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
                        check_fundef taint_inst name ctx ~glob_env
                          ?builtin_signature_db fdef
                      in
                      record_matches fdef_effects)
            ast;
          (None, None, false)
      in
      if needs_function_match_pass then
        check_function_defs_for_matches ~lang ~ctx ~taint_inst ~glob_env
          ?signature_db:final_signature_db ?builtin_signature_db
          ?call_graph:relevant_graph ~record_matches ast;

      (* Check execution of statements during object initialization. *)
      Visit_class_defs.visit
        (fun opt_ent cdef ->
          let opt_name =
            let* ent = opt_ent in
            AST_to_IL.name_of_entity ent
          in
          let fields =
            cdef.G.cbody |> Tok.unbracket
            |> List_.map (function G.F x -> x)
            |> G.stmt1
          in
          let stmts = AST_to_IL.stmt taint_inst.lang fields in
          let cfg, lambdas = CFG_build.cfg_of_stmts stmts in
          let init_effects, _mapping =
            Dataflow_tainting.fixpoint taint_inst ~in_env:imported_glob_env
              ?name:opt_name
              ?signature_db:final_signature_db ?builtin_signature_db
              ?call_graph:relevant_graph
              IL.{ params = []; cfg; lambdas }
          in
          record_matches init_effects)
        ast;

      (* Check the top-level statements.
       * In scripting languages it is not unusual to write code outside
       * function declarations and we want to check this too. We simply
       * treat the program itself as an anonymous function. *)
      let (), match_time =
        Common.with_time (fun () ->
            let xs = AST_to_IL.stmt taint_inst.lang (G.stmt1 ast) in
            let cfg, lambdas = CFG_build.cfg_of_stmts xs in
            let top_level_name =
              Graph_from_AST.top_level_il_name ~current_file:file ast
            in
            let top_effects, _mapping =
              Dataflow_tainting.fixpoint taint_inst ~in_env:imported_glob_env
                ~name:top_level_name
                ?signature_db:final_signature_db ?builtin_signature_db
                ?call_graph:relevant_graph
                IL.{ params = []; cfg; lambdas }
            in
            record_matches top_effects)
      in
      let matches =
        !matches
        (* same post-processing as for search-mode in Match_rules.ml *)
        |> PM.uniq
        |> PM.no_submatches (* see "Taint-tracking via ranges" *) |> match_hook
      in

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
    ?(interfile_context : interfile_context option)
    (rules : R.taint_rule list) (xconf : Match_env.xconfig)
    (xtarget : Xtarget.t) :
    Core_profiling.rule_profiling Core_result.match_result list =
  (* Check for language support warnings when taint_intrafile is enabled *)
  (Dataflow_tainting.reset_constructor ();
   match rules with
   | rule :: _ -> (
       let has_taint_intrafile =
         let xconf_rule =
           Match_env.adjust_xconfig_with_rule_options xconf rule.R.options
         in
         xconf_rule.config.taint_intrafile
       in
       if has_taint_intrafile then
         (* Warn for unsupported languages *)
         let lang = xtarget.xlang |> Xlang.to_lang_exn in
         match lang with
         | Lang.Apex
         | Lang.C
         | Lang.Clojure
         | Lang.Cpp
         | Lang.Csharp
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

  let has_rule_interfile_context rule =
    match interfile_context with
    | Some contexts -> Rule_ID.Map.mem (fst rule.R.id) contexts
    | None -> false
  in

  (* Collect all languages that have rules with taint_intrafile enabled *)
  let langs_needing_call_graph =
    rules
    |> List.fold_left
         (fun acc rule ->
            let xconf_rule =
              Match_env.adjust_xconfig_with_rule_options xconf rule.R.options
            in
            if xconf_rule.config.taint_intrafile
               && not (has_rule_interfile_context rule)
            then
              match Xlang.to_lang rule.R.target_analyzer with
              | Ok lang -> LangSet.add lang acc
              | Error _ -> acc
            else acc)
         LangSet.empty
  in

  (* Pre-compute call graph and builtin db for each language that needs it.
     The call graph depends on the AST structure and language, so we compute
     it once per language and share across rules that need it. *)
  let call_graph_by_lang =
    LangSet.fold
      (fun lang acc ->
        let ast, _skipped_tokens = lazy_force xtarget.lazy_ast_and_errors in
        let object_mappings =
          Taint_signature_extractor.detect_object_initialization ast lang
        in
        let call_graph =
          Graph_from_AST.build_call_graph ~lang ~object_mappings ast
        in
        LangMap.add lang (call_graph, object_mappings) acc)
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
           let rule_xconf =
             Match_env.adjust_xconfig_with_rule_options xconf rule.R.options
           in
           let rule_interfile_context =
             match (rule_xconf.config.interfile, interfile_context) with
             | true, Some contexts ->
                 Rule_ID.Map.find_opt (fst rule.R.id) contexts
             | _ -> None
           in
           let needs_shared_intrafile_setup =
             rule_xconf.config.taint_intrafile
             && Option.is_none rule_interfile_context
           in
           (* Only pass shared per-file intrafile state to rules that still
            * need it. Interfile rules with a prebuilt project context should
            * reuse that context directly. *)
           let rule_shared_call_graph, rule_builtin_signature_db =
             if needs_shared_intrafile_setup then
               match Xlang.to_lang rule.R.target_analyzer with
               | Ok lang ->
                   ( LangMap.find_opt lang call_graph_by_lang,
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
                       ?interfile_rule_context:rule_interfile_context
                       ~shared_call_graph:rule_shared_call_graph rule_xconf
                       xtarget
                   in
                   report)))
  in

  results
