(* projidx: language-agnostic project-wide symbol walker. *)

module G = AST_generic

open Types

module Log = Log_projidx.Log

module FA = Graph_from_AST

module Function_id_map = Map.Make (struct
  type t = Function_id.t
  let compare = Function_id.compare
end)

let build_funcs_by_id (all_funcs : FA.func_info list)
    : FA.func_info list Function_id_map.t =
  List.fold_left
    (fun (by_id : FA.func_info list Function_id_map.t) (func : FA.func_info) ->
      match Func_info.bare_name func.FA.fn_id with
      | None -> by_id
      | Some (name : IL.name) ->
        let id = Function_id.of_il_name name in
        Function_id_map.add id
          (func :: Option.value (Function_id_map.find_opt id by_id) ~default:[])
          by_id)
    Function_id_map.empty all_funcs

let build_class_parent_paths ~(entries : entry list)
    ~(funcs_by_id : FA.func_info list Function_id_map.t)
    : (Function_id.t * IL.name option list) list Common.SMap.t =
  List.fold_left
    (fun (paths : (Function_id.t * IL.name option list) list Common.SMap.t)
         (entry : entry) ->
      match (entry.kind, entry.defining_class_id) with
      | (K_function | K_class | K_companion), _
      | K_method, None -> paths
      | K_method, Some (class_id : Function_id.t) -> (
        match Function_id_map.find_opt entry.id funcs_by_id with
        | None
        | Some [] -> paths
        | Some ((func : FA.func_info) :: _) -> (
          match List_.init_and_last_opt func.FA.fn_id with
          | None
          | Some (_, None) -> paths
          | Some ((parent_path : IL.name option list), Some _) ->
            let key = Function_id.show class_id in
            let bound =
              Option.value (Common.SMap.find_opt key paths) ~default:[]
            in
            if
              List.exists
                (fun (((bound_id : Function_id.t), _) :
                        Function_id.t * IL.name option list) ->
                  Function_id.equal bound_id class_id)
                bound
            then paths
            else Common.SMap.add key ((class_id, parent_path) :: bound) paths)))
    Common.SMap.empty entries

let build_definitions_by_qn ~(entries : entry list)
    ~(funcs_by_id : FA.func_info list Function_id_map.t)
    ~(reexport_map : (Names.Module_qn.t, Names.Module_qn.t) Hashtbl.t)
    : definition Common.SMap.t =
  let functions =
    List.fold_left
      (fun (by_qn : definition Common.SMap.t) (entry : entry) ->
        match entry.kind with
        | K_class
        | K_companion -> by_qn
        | K_function
        | K_method -> (
          match Function_id_map.find_opt entry.id funcs_by_id with
          | None -> by_qn
          | Some (funcs : FA.func_info list) ->
            let key = Names.Def_qn.to_string entry.qn in
            let previous =
              match Common.SMap.find_opt key by_qn with
              | Some (Function_definitions earlier) -> earlier
              | Some (Class_definition _)
              | None -> []
            in
            Common.SMap.add key
              (Function_definitions (funcs @ previous)) by_qn))
      Common.SMap.empty entries
  in
  let companions : Names.Class_qn.t Common.SMap.t =
    List.fold_left
      (fun (companions : Names.Class_qn.t Common.SMap.t) (entry : entry) ->
        match entry.kind with
        | K_function
        | K_method
        | K_class -> companions
        | K_companion -> (
          match Names.Def_qn.split_last entry.qn with
          | None -> companions
          | Some ((parent : Names.Def_qn.t), _) ->
            Common.SMap.add
              (Names.Def_qn.to_string (Names.Def_qn.concat parent entry.name))
              (Names.Class_qn.of_string (Names.Def_qn.to_string entry.qn))
              companions))
      Common.SMap.empty entries
  in
  let with_classes =
    List.fold_left
      (fun (by_qn : definition Common.SMap.t) (entry : entry) ->
        match entry.kind with
        | K_function
        | K_method -> by_qn
        | K_class ->
          let qn = Names.Def_qn.to_string entry.qn in
          Common.SMap.add qn
            (Class_definition
               { class_file = entry.file;
                 class_qn = Names.Class_qn.of_string qn;
                 class_name = entry.name;
                 class_companion = Common.SMap.find_opt qn companions })
            by_qn
        | K_companion ->
          let qn = Names.Def_qn.to_string entry.qn in
          let definition =
            Class_definition
              { class_file = entry.file;
                class_qn = Names.Class_qn.of_string qn;
                class_name = entry.name;
                class_companion = None }
          in
          let by_qn = Common.SMap.add qn definition by_qn in
          match Names.Def_qn.split_last entry.qn with
          | None -> by_qn
          | Some ((parent : Names.Def_qn.t), _) ->
            let class_key =
              Names.Def_qn.to_string (Names.Def_qn.concat parent entry.name)
            in
            if Common.SMap.mem class_key by_qn then by_qn
            else Common.SMap.add class_key definition by_qn)
      functions entries
  in
  Hashtbl.fold
    (fun (bound : Names.Module_qn.t) (_target : Names.Module_qn.t)
         (by_qn : definition Common.SMap.t) ->
      let bound_key = Names.Module_qn.to_string bound in
        if Common.SMap.mem bound_key by_qn then by_qn
        else
          let is_known (qn : Names.Module_qn.t) : bool =
            Common.SMap.mem (Names.Module_qn.to_string qn) by_qn
          in
          match Mro.chase_reexport ~reexport_map ~is_known bound with
          | None -> by_qn
          | Some (target : Names.Module_qn.t) -> (
            match
              Common.SMap.find_opt (Names.Module_qn.to_string target) by_qn
            with
            | None -> by_qn
            | Some (found : definition) ->
              Common.SMap.add bound_key found by_qn))
    reexport_map with_classes


(* Maximum number of files processed per parallel work unit.  Batching
   amortises Domainslib dispatch overhead over many small per-file tasks
   while keeping [chunksize = 1] — one task per thread — so the
   [Memprof_limits]-based memory limit and timeout stay sound (see the
   warning on [Domainslib_.parmap]). *)
(* Files are sorted by decreasing size before the edge pass, so a batch is
   a run of similar-sized files; small batches let the pool balance the
   heavy head of that order instead of handing one domain the 500 largest
   files (on GitLab that left the pool one third busy). *)
let per_file_batch_size =
  match Sys.getenv_opt "OPENGREP_PROJIDX_BATCH" with
  | Some s -> ( try int_of_string s with _ -> 32)
  | None -> 32

(* Split a list into chunks of at most [n] elements. *)
let rec chunks (n : int) (xs : 'a list) : 'a list list =
  match xs with
  | [] -> []
  | _ ->
    let len = List.length xs in
    let take = min n len in
    let batch = List_.take_safe take xs in
    let rest = List_.drop take xs in
    batch :: chunks n rest

(* Run [fn] on each item, in parallel when [ncores > 1] and there is
   more than one batch, sequentially otherwise.  Each parallel work
   unit is a whole batch; inside a batch, a per-item failure becomes
   an [Error] for the caller to log and skip, while the fatal trio is
   re-raised. *)
let run_per_file (caps : < Cap.fork >) ~(ncores : int)
    (fn : 'a -> 'b) (items : 'a list)
    : ('b, 'a * Exception.t) Result.t list =
  (* [fn] fixes the type: one [Ok]/[Error] per item; an [Error] carries the
     item so the caller can attribute (and surface) the failure. The whole
     build runs under one memory limit scope, so the limit's exception
     must reach that scope. *)
  let run_one item =
    try Ok (fn item)
    with
    | (Out_of_memory | Memory_limit.ExceededMemoryLimit _) as exn ->
      Exception.catch_and_reraise exn
    | exn -> Error (item, Exception.catch exn)
  in
  let batches = chunks per_file_batch_size items in
  let n = List.length batches in
  if ncores <= 1 || n <= 1 then List_.map run_one items
  else
    Domainslib_.parmap caps
      ~num_domains:(min ncores n) ~chunksize:1
      ~exception_handler:(fun _ exn ->
        match Exception.get_exn exn with
        | Out_of_memory | Memory_limit.ExceededMemoryLimit _ ->
          Exception.reraise exn
        | _ -> exn)
      (fun batch -> List_.map run_one batch)
      batches
    |> List.map2 (fun batch -> function
        | Ok batch_results -> batch_results
        (* A batch-level failure (thrown outside [run_one]) loses the
           per-item results; attribute it to every item. *)
        | Error exn -> List_.map (fun item -> Error (item, exn)) batch)
        batches
    |> List.concat

(* Wall-clock time of one phase, at info level under one tag so a log grep
   gives the phase table (see also Interfile_dispatch.timed). *)
let timed (name : string) (f : unit -> 'a) : 'a =
  let res, secs = Common.with_time f in
  Log_interfile_timing.Log.info (fun m ->
      m "[interfile timing] project index: %s: %.2fs" name secs);
  res

let build_project_call_graph (caps : < Cap.fork >)
    ~(cfg : Index_lang_rules.t) ~(lang : Lang.t)
    ~(ncores : int) ~(entries : entry list)
    ?(class_infos = [])
    ?(reexport_map = Hashtbl.create 0)
    ~(go_packages : Scope_go.package_index)
    (file_infos : file_info list)
    : Call_graph.G.t * class_fun_info list * Core_error.t list =
  let skip_anon (opt_ent : G.entity option) =
    not cfg.Index_lang_rules.include_anonymous_funcs && Option.is_none opt_ent
  in
  let indexed_entries =
    List.filter
      (fun (entry : entry) ->
        not (cfg.Index_lang_rules.is_stub_file entry.file))
      entries
  in
  let indexed_classes =
    List.filter
      (fun (ci : class_info) ->
        not (cfg.Index_lang_rules.is_stub_file ci.ci_file))
      class_infos
  in
  let indexed_files =
    List.filter
      (fun (fi : file_info) ->
        not (cfg.Index_lang_rules.is_stub_file fi.fi_file))
      file_infos
  in
  (* The map from a child's simple name to its parent's simple name resolves
     [super().X()] to the parent's method. The map records the first parent's
     bare name only, which approximates single inheritance. *)
  let type_state =
    List.fold_left (fun state ci ->
      let child =
        match Names.Class_qn.bare_name ci.ci_qn with
        | "" -> None
        | bare_name -> Some bare_name
      in
      let parent =
        match ci.ci_parent_paths with
        | (first : Index_lang_rules.class_parent) :: _ ->
          (match List.rev first.Index_lang_rules.cp_path with
           | last :: _ -> Some last
           | [] -> None)
        | [] -> None
      in
      match child, parent with
      | Some child_name, Some parent_name ->
        let child = Names.Class_name.of_string child_name in
        Type_state.set_parent
          (Type_state.add_class_file state child ci.ci_file)
          child ci.ci_file (Names.Class_name.of_string parent_name)
      | Some child_name, None ->
        Type_state.add_class_file state
          (Names.Class_name.of_string child_name) ci.ci_file
      | None, _ -> state
    ) Type_state.empty class_infos
  in
  let graph = Call_graph.G.create () in
  (* Per-file synthetic [<top_level>] node for module-scope calls, so the dump
     keeps the caller's file/line. *)
  let top_level_nodes : (string, Function_id.t) Hashtbl.t =
    Hashtbl.create (List.length file_infos) in
  let top_level_node_for (file : Fpath.t) : Function_id.t =
    Hashtbl.find top_level_nodes (Fpath.to_string file)
  in

  let placeholder_fdef tok : G.function_definition = {
    G.fkind = (G.Method, tok);
    fparams = Tok.unsafe_fake_bracket [];
    frettype = None;
    fbody = G.FBNothing;
  } in
  let synth_func_info_for_class
      (class_il : IL.name) (name, tok) : FA.func_info =
    let m_il = IL.{
      ident = (name, tok);
      sid = G.SId.unsafe_default;
      id_info = G.empty_id_info ();
    } in
    { FA.fn_id = Func_info.method_id ~cls:class_il ~meth:m_il;
      entity = None;
      fdef = placeholder_fdef tok }
  in
  let phase1_per_file (fi : file_info) : FA.func_info list =
    (* Go interface methods come as a [TypeDef] with [TyRecordAnon(Interface)],
       not a [ClassDef], so attribute them to the interface. *)
    let interface_methods =
      if not (Lang.equal lang Lang.Go) then []
      else
        List.fold_left (fun acc obs ->
          match obs with
          | Walker.Observation.Type_def { ent; tdef } ->
            (match tdef with
             | { G.tbody = G.NewType
                   { G.t = G.TyRecordAnon ((G.Interface, _),
                                              (_, fields, _)); _ } } ->
               (match ent.G.name with
                | G.EN ((G.Id _ | G.IdQualified _) as iface_name) ->
                  let iface_il = AST_to_IL.var_of_name iface_name in
                  List.fold_left (fun acc field ->
                    match field with
                    | G.F { G.s = G.DefStmt (m_ent, G.FuncDef m_fdef); _ } ->
                      (match m_ent.G.name with
                       | G.EN ((G.Id _ | G.IdQualified _) as m_name) ->
                         let m_il = AST_to_IL.var_of_name m_name in
                         let fn_id = Func_info.method_id ~cls:iface_il ~meth:m_il in
                         { FA.fn_id;
                           entity = Some m_ent;
                           fdef = m_fdef } :: acc
                       | _ -> acc)
                    | _ -> acc
                  ) acc fields
                | _ -> acc)
             | _ -> acc)
          | _ -> acc
        ) [] fi.fi_observations
    in
    let attributed_to_interface (fn_id : FA.fn_id) : bool =
      match Func_info.bare_name fn_id with
      | None -> false
      | Some (name : IL.name) ->
        List.exists
          (fun (method_ : FA.func_info) ->
            match Func_info.bare_name method_.FA.fn_id with
            | None -> false
            | Some (other : IL.name) -> Function_id.equal_il_name name other)
          interface_methods
    in
    let acc =
      List.fold_left (fun (acc : FA.func_info list) obs ->
        match obs with
        | Walker.Observation.Func_def { opt_ent; parent_path; fdef } ->
          if skip_anon opt_ent then acc
          else
            (match FA.fn_id_of_entity ~lang opt_ent parent_path fdef with
             | Some fn_id when not (attributed_to_interface fn_id) ->
               { FA.fn_id; entity = opt_ent; fdef } :: acc
             | Some _
             | None -> acc)
        | _ -> acc
      ) interface_methods fi.fi_observations
    in
    List.fold_left (fun acc obs ->
      match obs with
      | Walker.Observation.Class_def { ent; cdef } ->
        (match Visit_function_defs.entity_to_il_name ent with
         | Some class_il ->
           List.fold_left (fun acc pair ->
             synth_func_info_for_class class_il pair :: acc
           ) acc (cfg.Index_lang_rules.class_body_synth_methods cdef)
         | None -> acc)
      | _ -> acc
    ) acc fi.fi_observations
  in
  let per_file_funcs =
    timed "call graph: functions per file" @@ fun () ->
    run_per_file caps ~ncores phase1_per_file file_infos
  in
  let all_funcs =
    List.concat_map (function Ok fs -> fs | Error _ -> []) per_file_funcs
  in
  (* Returned to the caller: a failed file's functions are MISSING from the
     graph, which silently loses every finding through them unless the
     failure is surfaced as a scan error. *)
  let phase1_failures =
    List.filter_map (function
      | Ok _ -> None
      | Error ((fi : file_info), exn) ->
        Log.warn (fun m -> m "[skip] projidx phase 1 failed on %s: %s"
                    (Fpath.to_string fi.fi_file) (Exception.to_string exn));
        Some (Core_error.exn_to_error ~file:fi.fi_file exn))
      per_file_funcs
  in
  List.iter (fun (func : FA.func_info) ->
    match FA.fn_id_to_node func.FA.fn_id with
    | Some node -> Call_graph.G.add_vertex graph node
    | None -> ()
  ) all_funcs;

  let project_class_names : G.name list =
    let seen : (string, unit) Hashtbl.t =
      Hashtbl.create (List.length class_infos) in
    List.fold_left (fun acc fi ->
      List.fold_left (fun acc name ->
        match name with
        | G.Id ((name_str, _), _) when not (Hashtbl.mem seen name_str) ->
          Hashtbl.replace seen name_str ();
          name :: acc
        | _ -> acc
      ) acc (Object_initialization.collect_class_names fi.fi_ast)
    ) [] file_infos
  in
  Log.debug (fun m -> m "Project class names: %d (interfile object_mappings)"
    (List.length project_class_names));

  let funcs_by_name : (string, FA.func_info list) Hashtbl.t =
    Hashtbl.create (List.length all_funcs * 2)
  in
  List.iter (fun (func : FA.func_info) ->
    let add_name name =
      let cur = Option.value (Hashtbl.find_opt funcs_by_name name) ~default:[] in
      Hashtbl.replace funcs_by_name name (func :: cur)
    in
    let bare_name =
      Option.map (fun name -> fst name.IL.ident)
        (Func_info.bare_name func.FA.fn_id)
    in
    Option.iter add_name bare_name;
    (* A named lambda carries the synthetic bare name [_tmp_lambda], so this
       code also indexes the lambda under the name of the variable it is bound
       to, and [handler(...)] then resolves. *)
    (match func.FA.entity with
     | Some ent ->
       (match Index_lang_rules.entity_simple_name ent with
        | Some entity_name when (match bare_name with
                                 | Some fn_bare_name ->
                                   not (String.equal fn_bare_name entity_name)
                                 | None -> true) ->
            add_name entity_name
        | _ -> ())
     | None -> ())
  ) all_funcs;
  let project_funcs_by_name = funcs_by_name in

  (* Class -> methods index, so a file that knows a class can resolve [d.speak]
     even if "speak" never appears in it. *)
  let type_state =
    List.fold_left (fun state (func : FA.func_info) ->
      match Func_info.as_method func.FA.fn_id with
      | Some (cls, _) ->
        Type_state.add_method state
          (Names.Class_name.of_string (fst cls.IL.ident)) func
      | None -> state
    ) type_state all_funcs
  in
  let type_state, inherited_by_class, override_pairs, class_resolution_orders =
    timed "call graph: inheritance (Mro)" @@ fun () ->
    if cfg.Index_lang_rules.walks_inheritance then
      Mro.inherit_into_type_state ~lang ~cfg ~reexport_map
        ~class_infos:indexed_classes
        ~func_def_file:Type_augment.func_def_file type_state
    else (type_state, [], [], [])
  in
  let resolution_orders : Func_lookup.resolution_orders =
    List.fold_left
      (fun (orders : Func_lookup.resolution_orders)
           ((class_qn : Names.Class_qn.t), (order : Names.Class_qn.t list)) ->
        Common.SMap.add (Names.Class_qn.to_string class_qn) order orders)
      Common.SMap.empty class_resolution_orders
  in
  let class_qn_by_definition : Func_lookup.class_qn_by_definition =
    List.fold_left
      (fun (by_name : Func_lookup.class_qn_by_definition) (ci : class_info) ->
        let name = Function_id.show ci.ci_id in
        Common.SMap.add name
          ((ci.ci_id, ci.ci_qn)
           :: Option.value (Common.SMap.find_opt name by_name) ~default:[])
          by_name)
      Common.SMap.empty indexed_classes
  in
  let classes_by_file : class_info list Common.SMap.t =
    List.fold_left
      (fun (by_file : class_info list Common.SMap.t) (ci : class_info) ->
        let key = Fpath.to_string ci.ci_file in
        Common.SMap.add key
          (ci :: Option.value (Common.SMap.find_opt key by_file) ~default:[])
          by_file)
      Common.SMap.empty indexed_classes
  in

  let type_state = Type_augment.populate_returns_from_decls type_state all_funcs in
  let type_state, slice_element_of_field =
    Type_augment.build_fields_by_class_index ~cfg type_state file_infos in
  (* Cross-type inference fixpoint: alternate body-return-types and
     self-assignment field-types until neither adds anything.  Rebuild
     [caller_arg_types] between passes so fresh return types feed the next;
     compare on [Type_state] only (the Hashtbl is derived). *)
  let uses_new_keyword = FA.uses_new_keyword lang in
  let outer_step (ts, _car) =
    let ts =
      Type_augment.augment_return_types_from_bodies ~uses_new_keyword ~type_state:ts all_funcs
    in
    let car =
      Type_augment.build_caller_arg_types ~uses_new_keyword ~type_state:ts file_infos
    in
    let ts =
      Type_augment.augment_fields_from_self_assignments ~lang ~uses_new_keyword
        ~caller_arg_types:car ~cfg ~type_state:ts all_funcs
    in
    (ts, car)
  in
  let outer_equal (a, _) (b, _) = Type_state.equal a b in
  let (type_state, caller_arg_types), outer_iters =
    timed "call graph: type inference fixpoint" @@ fun () ->
    Fixpoint.run
      ~equal:outer_equal
      ~step:outer_step
      ~max_iterations:Limits_semgrep.projidx_CALL_GRAPH_MAX_PASSES
      (type_state, Hashtbl.create 0)
  in
  Log.debug (fun m -> m "Body-inferred type fixpoint: %d outer passes, %d caller-arg-types"
    outer_iters (Hashtbl.length caller_arg_types));
  (* [Fixpoint.run] returns [i = max_iterations] only on the cap branch. *)
  if outer_iters >= Limits_semgrep.projidx_CALL_GRAPH_MAX_PASSES then
    Log.warn (fun m ->
        m "Body-inferred type fixpoint hit the %d-pass cap without \
           converging; inferred types may be incomplete"
          Limits_semgrep.projidx_CALL_GRAPH_MAX_PASSES);
  let type_state =
    Type_augment.build_module_singleton_types ~uses_new_keyword type_state file_infos
  in
  let t_indexes_start = Unix.gettimeofday () in
  let file_funcs_index = Type_augment.build_file_funcs_index all_funcs in
  Log.debug (fun m -> m "File-funcs index: %d files"
    (Hashtbl.length file_funcs_index));

  (* Project-wide free-function indexes.  See [Func_index]. *)
  let project_funcs_by_module =
    Func_index.build_by_module ~cfg ~file_infos all_funcs
  in
  Log.debug (fun m -> m "Per-module func index: %d modules (Per_file only)"
    (Hashtbl.length project_funcs_by_module));

  let dunder_all = Reexports.build_dunder_all ~file_infos in

  (* Re-export pass for the languages whose imports re-export.  See
     [Reexports]. *)
  if cfg.Index_lang_rules.has_reexports then
    Reexports.resolve_into_module_index ~cfg
      ~project_funcs_by_module ~dunder_all file_infos
    |> List.iter (fun (qn, funcs) ->
         Hashtbl.replace project_funcs_by_module qn funcs);

  (* Defining-file -> package module qn; disambiguates same-basename packages
     for resolving methods with the same simple name. *)
  let file_module_qn : (string, Names.Module_qn.t) Hashtbl.t =
    let index = Hashtbl.create (List.length file_infos) in
    List.iter (fun (fi : file_info) ->
      Hashtbl.replace index (Fpath.to_string fi.fi_file) fi.fi_module_path
    ) file_infos;
    index
  in

  (* Per-AST [extract_calls] into an edge list; graph mutated only in the
     merge step. *)
  Log_interfile_timing.Log.info (fun m ->
      m "[interfile timing] project index: call graph: indexes (exports, \
         packages, modules, re-exports, visibility): %.2fs"
        (Unix.gettimeofday () -. t_indexes_start));
  let funcs_by_id = build_funcs_by_id all_funcs in
  let definitions_by_qn =
    timed "call graph: definitions by qualified name" (fun () ->
      build_definitions_by_qn ~entries:indexed_entries ~funcs_by_id
        ~reexport_map)
  in
  let class_parent_paths =
    timed "call graph: class parent paths" (fun () ->
      build_class_parent_paths ~entries:indexed_entries ~funcs_by_id)
  in
  let methods_by_class : Func_lookup.methods_by_class =
    timed "call graph: methods by class" @@ fun () ->
    Common.SMap.fold
      (fun (qn : string) (definition : definition)
           (by_class : Func_lookup.methods_by_class) ->
        match definition with
        | Class_definition _ -> by_class
        | Function_definitions (funcs : FA.func_info list) -> (
          match Names.Def_qn.split_last (Names.Def_qn.of_string qn) with
          | None -> by_class
          | Some ((owner : Names.Def_qn.t), (name : string)) ->
            let owner_str = Names.Def_qn.to_string owner in
            if not (Common.SMap.mem owner_str resolution_orders) then by_class
            else
              let class_qn = Names.Class_qn.of_string owner_str in
              Func_lookup.Class_qn_map.add class_qn
                (Common.SMap.add name funcs
                   (Option.value
                      (Func_lookup.Class_qn_map.find_opt class_qn by_class)
                      ~default:Common.SMap.empty))
                by_class))
      definitions_by_qn Func_lookup.Class_qn_map.empty
  in
  let singleton_names : Func_lookup.singleton_names =
    timed "call graph: singleton method names by class" @@ fun () ->
    List.fold_left
      (fun (by_class : Func_lookup.singleton_names) (ci : class_info) ->
        let own_names () : unit Common.SMap.t =
          Common.SMap.map (fun _ -> ())
            (Option.value
               (Func_lookup.Class_qn_map.find_opt ci.ci_qn methods_by_class)
               ~default:Common.SMap.empty)
        in
        let exposed : unit Common.SMap.t option =
          match ci.ci_singleton_exposure with
          | Index_lang_rules.No_singleton_exposure -> None
          | Index_lang_rules.Every_method_is_a_singleton -> Some (own_names ())
          | Index_lang_rules.Named_singleton_methods (names : string list) ->
            Some
              (List.fold_left
                 (fun (set : unit Common.SMap.t) (name : string) ->
                   Common.SMap.add name () set)
                 Common.SMap.empty names)
        in
        match exposed with
        | None -> by_class
        | Some (names : unit Common.SMap.t) ->
          Func_lookup.Class_qn_map.add ci.ci_qn
            (Common.SMap.union (fun _ () () -> Some ()) names
               (Option.value
                  (Func_lookup.Class_qn_map.find_opt ci.ci_qn by_class)
                  ~default:Common.SMap.empty))
            by_class)
      Func_lookup.Class_qn_map.empty indexed_classes
  in
  let value_alias_index = Pipeline.build_value_alias_index file_infos in
  let module_scope =
    match cfg.Index_lang_rules.unqualified_scope with
    | `Per_module ->
      timed "call graph: module exports" (fun () ->
        Scope_module.build_project_scope ~definitions_by_qn ~value_alias_index
          ~classes_by_file ~class_parent_paths ~file_funcs_index
          ~file_infos:indexed_files)
    | `Per_file
    | `Per_crate
    | `Per_constant_path
    | `Per_directory
    | `Per_go_package
    | `Per_package
    | `Per_namespace
    | `Per_translation_unit
    | `Per_project -> Scope_module.no_project_scope
  in
  let methods_by_class : Func_lookup.methods_by_class =
    List.fold_left
      (fun (by_class : Func_lookup.methods_by_class)
           (((class_qn : Names.Class_qn.t), (name : string),
             (funcs : FA.func_info list))) ->
        Func_lookup.Class_qn_map.add class_qn
          (Common.SMap.add name funcs
             (Option.value
                (Func_lookup.Class_qn_map.find_opt class_qn by_class)
                ~default:Common.SMap.empty))
          by_class)
      methods_by_class (Scope_module.class_aliases_of module_scope)
  in
  let attributes_by_module =
    timed "call graph: attributes by module" (fun () ->
      Func_index.build_attributes_by_module ~cfg ~dunder_all
        ~exported:
          (match cfg.Index_lang_rules.unqualified_scope with
           | `Per_module ->
             Func_index.Only_exported_names
               (Scope_module.exported_names
                  (Scope_module.exports_of module_scope))
           | `Per_file
           | `Per_crate
           | `Per_constant_path
           | `Per_directory
           | `Per_go_package
           | `Per_package
           | `Per_namespace
           | `Per_translation_unit
           | `Per_project ->
             Func_index.Every_definition_is_an_attribute)
        ~definitions_by_qn ~file_infos:indexed_files)
  in
  let pipeline_ctx : Pipeline.ctx =
    { Pipeline.lang;
      cfg;
      type_state;
      definitions_by_qn;
      attributes_by_module;
      namespace_scope_bindings =
        timed "call graph: namespace bindings" (fun () ->
          match cfg.Index_lang_rules.unqualified_scope with
          | `Per_namespace
          | `Per_translation_unit ->
            Scope_binding.build_namespace_scope_bindings ~attributes_by_module
              ~file_infos:indexed_files
          | `Per_file
          | `Per_crate
          | `Per_constant_path
          | `Per_directory
          | `Per_go_package
          | `Per_module
          | `Per_project
          | `Per_package -> Common.SMap.empty);
      php_global_bindings =
        (match cfg.Index_lang_rules.unqualified_scope with
         | `Per_namespace ->
           Scope_php.global_function_bindings ~attributes_by_module
         | `Per_file
         | `Per_crate
         | `Per_constant_path
         | `Per_directory
         | `Per_go_package
         | `Per_module
         | `Per_package
         | `Per_translation_unit
         | `Per_project -> []);
      include_map =
        timed "call graph: include closures" (fun () ->
          match cfg.Index_lang_rules.unqualified_scope with
          | `Per_translation_unit ->
            Include_map.build ~file_infos:indexed_files ~file_funcs_index
          | `Per_file
          | `Per_crate
          | `Per_constant_path
          | `Per_directory
          | `Per_go_package
          | `Per_module
          | `Per_namespace
          | `Per_project
          | `Per_package -> Include_map.empty);
      module_scope;
      go_packages;
      top_level_scope =
        timed "call graph: top level constants" (fun () ->
          match cfg.Index_lang_rules.unqualified_scope with
          | `Per_constant_path
          | `Per_project ->
            Func_lookup.scope_table_of_map
              (Scope_binding.bindings_of_positioned
                 (Scope_binding.top_level_bindings
                    ~keep:(fun (func : Func_info.t) ->
                      cfg.Index_lang_rules.project_scope_admits
                        func.Func_info.entity)
                    ~definitions_by_qn))
          | `Per_file
          | `Per_crate
          | `Per_directory
          | `Per_go_package
          | `Per_module
          | `Per_namespace
          | `Per_package
          | `Per_translation_unit -> Func_lookup.empty_scope_table);
      namespace_object_members =
        (if cfg.Index_lang_rules.object_members_bind_in_namespace then
           let objects : unit Common.SMap.t =
             List.fold_left
               (fun (objects : unit Common.SMap.t) (ci : class_info) ->
                 match ci.ci_class_kind with
                 | G.Object ->
                   Common.SMap.add (Names.Class_qn.to_string ci.ci_qn) ()
                     objects
                 | G.Class
                 | G.Interface
                 | G.Trait -> objects)
               Common.SMap.empty indexed_classes
           in
           Common.SMap.filter_map
             (fun (_ : string)
                  (attributes : Func_lookup.module_attribute Common.SMap.t) ->
               match
                 Common.SMap.fold
                   (fun (_ : string)
                        (attribute : Func_lookup.module_attribute)
                        (lifted : Scope_binding.positioned_binding list) ->
                     match attribute with
                     | Func_lookup.Attr_class (class_qn : Names.Class_qn.t)
                       when Common.SMap.mem
                              (Names.Class_qn.to_string class_qn) objects ->
                       List.concat_map
                         (fun ((name : string), (funcs : Func_info.t list)) ->
                           Scope_binding.function_binding_of ~pos:None
                             ~parent_path:[] name funcs)
                         (Scope_package.members_along_order
                            ~resolution_orders ~methods_by_class class_qn)
                       @ lifted
                     | Func_lookup.Attr_class _
                     | Func_lookup.Attr_class_with_companion _
                     | Func_lookup.Attr_functions _
                     | Func_lookup.Attr_module _ -> lifted)
                   attributes []
               with
               | [] -> None
               | lifted -> Some lifted)
             attributes_by_module
         else Common.SMap.empty);
      dunder_all;
      resolution_orders;
      class_qn_by_definition;
      methods_by_class;
      singleton_names;
      extensions_by_module =
        timed "call graph: extension methods by module" (fun () ->
          Func_index.build_extensions_by_module ~definitions_by_qn);
      nested_types_by_class =
        timed "call graph: nested types by class" (fun () ->
          Func_index.build_nested_types_by_class ~definitions_by_qn);
      classes_by_file;
      class_parent_paths;
      global_imports =
        List.concat_map
          (fun (fi : file_info) ->
            List.filter (fun (imp : import) -> imp.im_global) fi.fi_imports)
          indexed_files;
      project_funcs_by_name;
      project_funcs_by_module;
      file_module_qn;
      project_class_names;
      file_funcs_index;
      slice_element_of_field;
      top_level_node_for;
      stamp_var_types =
        (fun ~type_state ~slice_element_of_field ast ->
          Type_augment.stamp_var_types_from_bodies ~uses_new_keyword
            ~type_state ~slice_element_of_field ast);
      value_alias_index;
    }
  in
  (* per-file wall time of the edge walk, across domains, for the slowest
     files: the pass is parallel, so a few slow files bound its wall time *)
  let file_secs : (Fpath.t * float) list ref = ref [] in
  let file_secs_mutex = Mutex.create () in
  let edges_for_file fi =
    let res, secs =
      Common.with_time (fun () -> Pipeline.edges_for_file pipeline_ctx fi)
    in
    Mutex.lock file_secs_mutex;
    file_secs := (fi.fi_file, secs) :: !file_secs;
    Mutex.unlock file_secs_mutex;
    res
  in
  (* Pre-populate [<top_level>] nodes BEFORE the parallel phase: the table and
     graph are read-only across domains after this. *)
  List.iter (fun (fi : file_info) ->
    let key = Fpath.to_string fi.fi_file in
    if not (Hashtbl.mem top_level_nodes key) then begin
      let node =
        Function_id.of_il_name (FA.top_level_name_of_ast fi.fi_ast)
      in
      Hashtbl.replace top_level_nodes key node;
      Call_graph.G.add_vertex graph node
    end
  ) file_infos;
  (* Largest-first so megafiles don't stall the tail on one worker; stat once
     (decorate-sort-undecorate) to keep the comparator pure. *)
  let file_size_of fi =
    Nonfatal.catch ~default:0 (fun () ->
      (Unix.stat (Fpath.to_string fi.fi_file)).Unix.st_size)
  in
  let file_infos =
    file_infos
    |> List_.map (fun fi -> (file_size_of fi, fi))
    |> List.sort (fun (a, _) (b, _) -> Int.compare b a)
    |> List_.map snd
  in
  let per_file_edges =
    timed "call graph: edges per file" @@ fun () ->
    run_per_file caps ~ncores edges_for_file file_infos
  in
  (* stage split of the edge pass, and the shape of the name table it
     resolves calls against *)
  Log_interfile_timing.Log.info (fun m ->
      let stages =
        Pipeline.edge_stage_report ()
        |> List.map (fun (name, secs) -> Printf.sprintf "%s %.1fs" name secs)
        |> String.concat ", "
      in
      let n_names = Hashtbl.length project_funcs_by_name in
      let sizes =
        Hashtbl.fold
          (fun name funcs acc -> (name, List.length funcs) :: acc)
          project_funcs_by_name []
        |> List.sort (fun (_, a) (_, b) -> compare b a)
      in
      let total = List.fold_left (fun acc (_, n) -> acc + n) 0 sizes in
      let top =
        List.filteri (fun i _ -> i < 8) sizes
        |> List.map (fun (name, n) -> Printf.sprintf "%s:%d" name n)
        |> String.concat " "
      in
      m "[interfile timing] project index: edge pass stages (CPU sum): %s; \
         name table: %d names, %d funcs, largest buckets %s"
        stages n_names total top);
  Log_interfile_timing.Log.info (fun m ->
      let slowest =
        List.sort (fun (_, a) (_, b) -> compare b a) !file_secs
        |> List.filteri (fun i _ -> i < 8)
        |> List.map (fun (file, secs) ->
               Printf.sprintf "%s %.1fs" (Fpath.to_string file) secs)
        |> String.concat ", "
      in
      let total = List.fold_left (fun acc (_, s) -> acc +. s) 0. !file_secs in
      m "[interfile timing] project index: edge pass in-domain total %.1fs \
         over %d files (compare with the stage sum: the rest is outside the \
         timed stages); slowest files: %s"
        total (List.length !file_secs) slowest);
  (* A failed file's outgoing call edges are MISSING from the graph; the
     failure list is returned so the engine can surface it as a scan error. *)
  let t_merge_start = Unix.gettimeofday () in
  let n_emitted =
    List.fold_left
      (fun n -> function Ok edges -> n + List.length edges | Error _ -> n)
      0 per_file_edges
  in
  let phase2_failures =
    timed (Printf.sprintf "call graph: add call edges (%d emitted)" n_emitted)
    @@ fun () ->
    List.filter_map (function
      | Ok edges ->
        List.iter (fun (src, dst, call_tok) ->
          Call_graph.add_edge graph ~src ~dst ~call_tok)
          edges;
        None
      | Error ((fi : file_info), exn) ->
        Log.warn (fun m -> m "[skip] projidx phase 2 failed on %s: %s"
                    (Fpath.to_string fi.fi_file) (Exception.to_string exn));
        Some (Core_error.exn_to_error ~file:fi.fi_file exn))
      per_file_edges
  in
  let type_key : file:string option -> G.type_ -> string option =
    match cfg.Index_lang_rules.unqualified_scope with
    | `Per_file
    | `Per_crate
    | `Per_constant_path
    | `Per_directory
    | `Per_module
    | `Per_namespace
    | `Per_package
    | `Per_translation_unit
    | `Per_project ->
      fun ~file:_ (ty : G.type_) ->
        Option.bind (Ty_bare_name.class_name_of_ty ty)
          Ty_bare_name.bare_name_of_name
    | `Per_go_package ->
      let known : (string, unit) Hashtbl.t =
        Hashtbl.create (List.length indexed_classes)
      in
      List.iter
        (fun (ci : class_info) ->
          Hashtbl.replace known (Names.Class_qn.to_string ci.ci_qn) ())
        indexed_classes;
      let scope_of_file : (string * Names.Module_qn.t Common.SMap.t)
                            Common.SMap.t =
        List.fold_left
          (fun (by_file :
                  (string * Names.Module_qn.t Common.SMap.t) Common.SMap.t)
               (fi : file_info) ->
            Common.SMap.add (Fpath.to_string fi.fi_file)
              (Names.Module_qn.to_string fi.fi_module_path,
               Scope_go.import_aliases fi)
              by_file)
          Common.SMap.empty indexed_files
      in
      fun ~(file : string option) (ty : G.type_) ->
        Option.bind (Ty_bare_name.qualified_class_name_of_ty ty)
          (fun (name : G.name) ->
            Option.bind (Ty_bare_name.bare_name_of_name name)
              (fun (bare_name : string) ->
                let scope =
                  Option.bind file
                    (fun (file : string) ->
                      Common.SMap.find_opt file scope_of_file)
                in
                match (Ty_bare_name.qualifier_of_name name, scope) with
                | None, Some ((package : string), _) ->
                  let candidate =
                    Names.Class_qn.to_string
                      (Names.Class_qn.concat
                         (Names.Class_qn.of_string package) bare_name)
                  in
                  if Hashtbl.mem known candidate then Some candidate
                  else Some bare_name
                | None, None -> Some bare_name
                | Some (qualifier : string), Some (_, (aliases : _)) -> (
                  match Common.SMap.find_opt qualifier aliases with
                  | Some (target : Names.Module_qn.t) ->
                    Some
                      (Names.Class_qn.to_string
                         (Names.Class_qn.concat
                            (Names.Class_qn.of_string
                               (Names.Module_qn.to_string target))
                            bare_name))
                  | None -> Some (qualifier ^ "." ^ bare_name))
                | Some (qualifier : string), None ->
                  Some (qualifier ^ "." ^ bare_name)))
  in
  (* Interface dispatch edges.  See [Structural_dispatch]. *)
  let n_dispatch =
    timed "call graph: interface dispatch edges" @@ fun () ->
    Structural_dispatch.emit_dispatch_edges
      ~lang ~cfg ~type_state ~func_def_file:Type_augment.func_def_file
      ~type_key ~class_infos ~graph
  in
  if n_dispatch > 0 then
    Log.debug (fun m -> m "Interface dispatch: emitted %d Dispatch edges"
      n_dispatch);
  (* Nominal override dispatch: a subclass method shadowing a body-less
     ancestor decl (abstract method).  Same edge shape as interface
     dispatch (impl -> decl), so [dispatch_merge_fbdecl] and the
     reachability dispatch closure treat both alike. *)
  let n_override =
    timed "call graph: override dispatch edges" @@ fun () ->
    List.fold_left
      (fun n ((c_m : FA.func_info), (p_m : FA.func_info)) ->
        match FA.fn_id_to_node c_m.FA.fn_id, FA.fn_id_to_node p_m.FA.fn_id with
        | Some src, Some dst ->
          let call_tok =
            match c_m.FA.fn_id with
            | [_; Some m_il] -> snd m_il.IL.ident
            | _ -> snd c_m.FA.fdef.G.fkind
          in
          Call_graph.add_edge ~kind:Call_graph.Dispatch graph
            ~src ~dst ~call_tok;
          n + 1
        | _ -> n)
      0 override_pairs
  in
  if n_override > 0 then
    Log.debug (fun m -> m "Override dispatch: emitted %d Dispatch edges"
      n_override);
  (* Same-arity overloads of one scope: see [Structural_dispatch]. *)
  let n_overload =
    timed "call graph: overload dispatch edges" @@ fun () ->
    Structural_dispatch.emit_overload_edges ~lang ~graph
      ~class_qn_by_definition all_funcs
  in
  if n_overload > 0 then
    Log.debug (fun m -> m "Overload dispatch: emitted %d Dispatch edges"
      n_overload);
  Log_interfile_timing.Log.info (fun m ->
      m "[interfile timing] project index: call graph: edge merge (add edges, \
         dispatch/override/overload): %.2fs"
        (Unix.gettimeofday () -. t_merge_start));
  (graph, inherited_by_class, phase1_failures @ phase2_failures)

let project_root_abs_of (project_root : Fpath.t) : Fpath.t =
  fst (Fpath_.absolutify ~cwd:(Fpath.v (Sys.getcwd ())) project_root)

let run_pipeline (caps : < Cap.fork >)
    ?(targeting_conf : Find_targets.conf =
                Discover.projidx_default_targeting_conf)
    ~(lang : Lang.t) ~(project_root : Fpath.t) ~(ncores : int)
    ~(includes : string list) ~(excludes : string list) ()
  : entry list * Call_graph.G.t * int * int * file_info list
    * Core_error.t list =
  let cfg = Index_lang_rules.for_lang lang in
  (* Absolutize paths: interface dispatch's [family_key] needs consistent
     directory prefixes. *)
  let project_root_abs = project_root_abs_of project_root in
  (* One walk of the build configuration; its excludes keep the CLI and the
     embedded engine indexing the same files. *)
  let discovered =
    cfg.Index_lang_rules.discover_project ~project_root:project_root_abs
  in
  let excludes = excludes @ discovered.Index_lang_rules.excludes in
  let files =
    timed "discover files" @@ fun () ->
    Discover.discover_files ~targeting_conf
      ~lang ~project_root ~includes ~excludes
  in
  let n_total = List.length files in
  Log.info (fun m -> m "Discovered %d %s files. Parsing with %d domain(s)..."
    n_total (Lang.to_string lang) ncores);
  (* Rust only: [impl Foo {...}] ([OtherDef("Impl")]) is rewritten into a
     [ClassDef] in the STORED ast ([cfg.class_def_reshape]) so that every
     later pass sees the methods as class methods; every other language that
     wires the hook, Go among them, applies it to the collector's view alone
     and keeps the stored ast as the parser produced it. *)
  let rec reshape_class_defs (ast : G.program) : G.program =
    if not (Lang.equal lang Lang.Rust) then ast
    else
      List.map (fun (stmt : G.stmt) ->
        match stmt.G.s with
        | G.DefStmt (ent, G.ModuleDef { G.mbody = G.ModuleStruct (name, items) })
          ->
          { stmt with
            G.s =
              G.DefStmt
                (ent,
                 G.ModuleDef
                   { G.mbody =
                       G.ModuleStruct (name, reshape_class_defs items) }) }
        | G.DefStmt (ent, def_kind) ->
          (match cfg.Index_lang_rules.class_def_reshape ent def_kind with
           | Some (new_ent, new_kind) ->
             { stmt with G.s = G.DefStmt (new_ent, new_kind) }
           | None -> stmt)
        | _ -> stmt
      ) ast
  in
  let absolutize (file : Fpath.t) : Fpath.t =
    fst (Fpath_.absolutify ~cwd:project_root_abs file)
  in
  (* Go package identity from [go.mod] (go.work workspaces are not
     parsed); empty for non-Go, so [mp]
     falls back to the path-derived default. *)
  let go_modules =
    if Lang.equal lang Lang.Go
    then Go_modules.discover ~project_root:project_root_abs
           (List.map absolutize files)
    else Go_modules.empty
  in
  let rust_crates =
    if Lang.equal lang Lang.Rust
    then Rust_crates.discover ~project_root:project_root_abs
           (List.map absolutize files)
    else Rust_crates.empty
  in
  let resolution =
    if cfg.Index_lang_rules.specifiers_name_files then
      timed "specifier resolution" @@ fun () ->
      Module_paths.specifier_resolution_of_files ~cfg
        ~project_root:project_root_abs
        ~paths:discovered.Index_lang_rules.module_paths
        (List.map absolutize files)
    else Module_paths.Specifier_is_module_name
  in
  let process file =
    let file = absolutize file in
    let ast =
      Parse_target.parse_and_resolve_name_warn_if_partial lang file
    in
    let ast = reshape_class_defs ast in
    let mp =
      Module_paths.module_qn_of_file ~cfg ~go_modules ~rust_crates
        ~project_root ~ast:(Some ast) file
    in
    Symbols.collect_in_ast ~cfg ~lang ~resolution ~module_path:mp ~file ast
  in
  let results =
    timed (Printf.sprintf "parse + symbols (%d files)" n_total) @@ fun () ->
    if ncores <= 1 then
      List.map (fun file ->
        try Ok (process file)
        with
        | (Out_of_memory | Memory_limit.ExceededMemoryLimit _) as exn ->
          Exception.catch_and_reraise exn
        | exn -> Error (file, Exception.catch exn)
      ) files
    else
      Domainslib_.parmap caps
        ~num_domains:ncores
        ~chunksize:1
        ~exception_handler:(fun file exc ->
          match Exception.get_exn exc with
          | Out_of_memory | Memory_limit.ExceededMemoryLimit _ ->
            Exception.reraise exc
          | _ -> (file, exc))
        process
        files
  in
  let scanned, skipped, all_entries, all_classes, all_files, parse_failures =
    List.fold_left (fun (sc, sk, es, cs, fis, fails) -> function
      | Ok (entries, class_infos, fi) ->
        (sc + 1, sk,
         List.rev_append entries es,
         List.rev_append class_infos cs,
         fi :: fis,
         fails)
      | Error (file, exn) ->
        (* [sk] counts failures so far; log the first five only. *)
        if sk < 5 then
          Log.warn (fun m -> m "[skip] %s: %s" (Fpath.to_string file)
                      (Exception.to_string exn));
        (sc, sk + 1, es, cs, fis, Core_error.exn_to_error ~file exn :: fails)
    ) (0, 0, [], [], [], []) results
  in
  let parse_failures = List.rev parse_failures in
  let go_packages =
    timed "Go package index" @@ fun () ->
    Scope_go.build_package_index ~cfg ~file_infos:all_files
  in
  let all_files, all_classes =
    timed "Go import local names" @@ fun () ->
    Imports.with_package_clause_locals ~cfg
      ~clause_of_module:(Scope_go.importable_clause go_packages)
      (all_files, all_classes)
  in
  let reexport_map =
    timed "re-export map" @@ fun () ->
    Reexports.build_reexport_map ~cfg all_files
  in
  Log.debug (fun m -> m "Re-export map: %d entries (lang has_reexports=%b)"
    (Hashtbl.length reexport_map) cfg.Index_lang_rules.has_reexports);
  let wrappers : (string, dataclass_wrapper) Hashtbl.t =
    Hashtbl.create 64 in
  List.iter (fun fi ->
    List.iter (fun (wrapper : Index_lang_rules.wrapper) ->
      Hashtbl.replace wrappers wrapper.w_simple_name wrapper
    ) fi.fi_dataclass_wrappers
  ) all_files;
  Log.debug (fun m -> m "Wrappers: %d" (Hashtbl.length wrappers));
  let synth_from_wrappers =
    timed "wrapper synthesis" @@ fun () ->
    Symbols.dataclass_wrapper_synth_entries ~cfg ~wrappers all_entries all_classes
  in
  Log.debug (fun m -> m "Wrapper synthesis: %d dunders emitted"
    (List.length synth_from_wrappers));
  let entries_pre_mro = all_entries @ synth_from_wrappers in
  let graph, inherited_by_class, worker_failures =
    timed "call graph (edges + fixpoint)" @@ fun () ->
    build_project_call_graph caps ~cfg ~lang ~ncores ~entries:entries_pre_mro
      ~class_infos:all_classes ~reexport_map ~go_packages all_files
  in
  (* Inherited-method entry rows, derived from the same C3 linearisation
     callee resolution reads, so the diagnostic dump matches what resolution
     sees.  Only [collect] consumers use these rows; [collect_resolved]
     discards them.  The derivation is one pass over the C3 output; consider
     gating it to the [collect] path if it ever shows up in profiles. *)
  let inherited =
    timed "inherited method entries" @@ fun () ->
    List.concat_map (fun ((ci : class_info), funcs) ->
      List.filter_map (fun (func : Func_info.t) ->
        Option.map (fun (meth : IL.name) ->
          let method_name = fst meth.IL.ident in
          { id = Symbols.synth_function_id ci.ci_id method_name;
            name = method_name;
            qn = Names.Def_qn.concat
                   (Names.Def_qn.of_string (Names.Class_qn.to_string ci.ci_qn))
                   method_name;
            kind = K_method;
            file = ci.ci_file; range = ci.ci_range;
            defining_class_id = Some ci.ci_id })
          (Func_info.bare_name func.Func_info.fn_id))
        funcs)
      inherited_by_class
  in
  Log.debug (fun m -> m "Inheritance: %d inherited method entries (lang walks_inheritance=%b)"
    (List.length inherited) cfg.Index_lang_rules.walks_inheritance);
  let final_entries = entries_pre_mro @ inherited in
  Log.info (fun m -> m "Call graph: %d vertices, %d edges"
    (Call_graph.G.nb_vertex graph) (Call_graph.G.nb_edges graph));
  (final_entries, graph, scanned, skipped, all_files,
   parse_failures @ worker_failures)

let collect (caps : < Cap.fork >)
    ?(targeting_conf : Find_targets.conf =
                Discover.projidx_default_targeting_conf)
    ~(lang : Lang.t) ~(project_root : Fpath.t) ~(ncores : int)
    ~(includes : string list) ~(excludes : string list) ()
  : entry list * Call_graph.G.t * int * int =
  let (entries, graph, scanned, skipped, _all_files, _failures) =
    run_pipeline caps ~targeting_conf ~lang ~project_root ~ncores
      ~includes ~excludes ()
  in
  (entries, graph, scanned, skipped)

let collect_resolved (caps : < Cap.fork >)
    ?(targeting_conf : Find_targets.conf =
                Discover.projidx_default_targeting_conf)
    ~(lang : Lang.t) ~(project_root : Fpath.t) ~(ncores : int)
    ~(includes : string list) ~(excludes : string list) ()
  : Call_graph.G.t * (string, G.program) Hashtbl.t * Core_error.t list =
  let project_root_abs = project_root_abs_of project_root in
  let absnorm (file : Fpath.t) : string =
    fst (Fpath_.absolutify ~cwd:project_root_abs file) |> Fpath.to_string
  in
  let (_entries, graph, _scanned, _skipped, all_files, failures) =
    run_pipeline caps ~targeting_conf ~lang ~project_root:project_root_abs
      ~ncores ~includes ~excludes ()
  in
  let tbl = Hashtbl.create (List.length all_files) in
  List.iter (fun (fi : file_info) ->
    Hashtbl.replace tbl (absnorm fi.fi_file) fi.fi_ast)
    all_files;
  (graph, tbl, failures)

let resolve_ast_for_file (caps : < Cap.fork >)
    ?(targeting_conf : Find_targets.conf =
                Discover.projidx_default_targeting_conf)
    ~(lang : Lang.t) ~(project_root : Fpath.t) ~(ncores : int)
    ~(target : Fpath.t) ()
  : G.program option =
  let project_root_abs = project_root_abs_of project_root in
  let target_key =
    fst (Fpath_.absolutify ~cwd:project_root_abs target) |> Fpath.to_string
  in
  let _graph, asts, _failures =
    collect_resolved caps ~targeting_conf ~lang ~project_root ~ncores
      ~includes:[] ~excludes:[] ()
  in
  Hashtbl.find_opt asts target_key
