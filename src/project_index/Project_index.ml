(* projidx: language-agnostic project-wide symbol walker. *)

module G = AST_generic

open Types

module Log = Log_projidx.Log

let module_path ~(cfg : Index_lang_rules.t) ~(project_root : Fpath.t)
    ?(ast : G.program option) (file : Fpath.t) : Names.Module_qn.t =
  match Option.bind ast cfg.Index_lang_rules.module_path_from_ast with
  | Some mod_str -> Names.Module_qn.of_string mod_str
  | None ->
    let rel = Discover.relative_to ~project_root file in
    let path_str = Fpath.rem_ext rel |> Fpath.normalize |> Fpath.to_string in
    let path_str = cfg.Index_lang_rules.rewrite_module_path path_str in
    Names.Module_qn.of_string
      (String.concat "." (Fpath.segs (Fpath.v path_str)))
module FA = Graph_from_AST


(* Maximum number of files processed per parallel work unit.  Batching
   amortises Domainslib dispatch overhead over many small per-file tasks
   while keeping [chunksize = 1] — one task per thread — so the
   [Memprof_limits]-based memory limit and timeout stay sound (see the
   warning on [Domainslib_.parmap]). *)
let per_file_batch_size = 500

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
    : ('b, string) Result.t list =
  (* [fn] fixes the type: one [Ok]/[Error] per item. *)
  let run_one item =
    try Ok (fn item)
    with
    | (Out_of_memory | Stack_overflow | Time_limit.Timeout _) as exn ->
      Exception.catch_and_reraise exn
    | exn -> Error (Printexc.to_string exn)
  in
  let batches = chunks per_file_batch_size items in
  let n = List.length batches in
  if ncores <= 1 || n <= 1 then List_.map run_one items
  else
    Domainslib_.parmap caps
      ~num_domains:(min ncores n) ~chunksize:1
      ~exception_handler:(fun _ exn ->
        match Exception.get_exn exn with
        | Out_of_memory | Stack_overflow | Time_limit.Timeout _ ->
          Exception.reraise exn
        | exn -> Printexc.to_string exn)
      (fun batch -> List_.map run_one batch)
      batches
    |> List.concat_map (function
        | Ok batch_results -> batch_results
        | Error msg -> [ Error msg ])

let build_project_call_graph (caps : < Cap.fork >)
    ~(cfg : Index_lang_rules.t) ~(lang : Lang.t)
    ~(ncores : int)
    ?(class_infos = [])
    ?(reexport_map = Hashtbl.create 0)
    (file_infos : file_info list) : Call_graph.G.t * class_fun_info list =
  let skip_anon (opt_ent : G.entity option) =
    not cfg.Index_lang_rules.include_anonymous_funcs && Option.is_none opt_ent
  in
  (* [child_simple_name -> parent_simple_name] resolves [super().X()] to the
     parent's method (single-inheritance approximation: first parent's leaf). *)
  let type_state =
    List.fold_left (fun state ci ->
      let child =
        match Names.Class_qn.leaf ci.ci_qn with
        | "" -> None
        | leaf -> Some leaf
      in
      let parent =
        match ci.ci_parent_paths with
        | first :: _ ->
          (match List.rev first with
           | last :: _ -> Some last
           | [] -> None)
        | [] -> None
      in
      match child, parent with
      | Some child_name, Some parent_name ->
        Type_state.set_parent state
          (Names.Class_name.of_string child_name)
          (Names.Class_name.of_string parent_name)
      | _ -> state
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
    let acc =
      List.fold_left (fun (acc : FA.func_info list) obs ->
        match obs with
        | Walker.Observation.Func_def { opt_ent; parent_path; fdef } ->
          if skip_anon opt_ent then acc
          else
            (match FA.fn_id_of_entity ~lang opt_ent parent_path fdef with
             | Some fn_id -> { FA.fn_id; entity = opt_ent; fdef } :: acc
             | None -> acc)
        | _ -> acc
      ) [] fi.fi_observations
    in
    (* Go interface methods come as a [TypeDef] with [TyRecordAnon(Interface)],
       not a [ClassDef], so attribute them to the interface. *)
    let acc =
      if not (Lang.equal lang Lang.Go) then acc
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
        ) acc fi.fi_observations
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
  let per_file_funcs = run_per_file caps ~ncores phase1_per_file file_infos in
  let all_funcs =
    List.concat_map (function
      | Ok fs -> fs
      | Error msg ->
        Log.warn (fun m -> m "[skip] phase 1 worker failed: %s" msg);
        []) per_file_funcs
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
    let leaf = Option.map (fun name -> fst name.IL.ident) (Func_info.leaf_name func.FA.fn_id) in
    Option.iter add_name leaf;
    (* Named lambdas have a synthetic [_tmp_lambda] leaf; also index under the
       binding var name so [handler(...)] resolves. *)
    (match func.FA.entity with
     | Some ent ->
       (match Index_lang_rules.entity_simple_name ent with
        | Some entity_name when (match leaf with
                                 | Some leaf_name -> not (String.equal leaf_name entity_name)
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
  let type_state =
    Go_inheritance.lift_embedded_interfaces ~lang file_infos type_state
  in
  let type_state, inherited_by_class =
    if cfg.Index_lang_rules.walks_inheritance then
      let cross_module_parents =
        match cfg.Index_lang_rules.unqualified_scope with
        | `Per_file -> false
        | `Per_directory | `Per_package -> true
      in
      Mro.inherit_into_type_state ~cross_module_parents ~reexport_map
        ~class_infos ~func_def_file:Type_augment.func_def_file type_state
    else (type_state, [])
  in

  (* TS/JS class-body aliases [class C { static foo = importedFn }]. *)
  let type_state =
    Ts_class_aliases.add_class_body_aliases
      ~lang ~project_funcs_by_name file_infos type_state
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
    Fixpoint.run
      ~equal:outer_equal
      ~step:outer_step
      ~max_iterations:Limits_semgrep.projidx_CALL_GRAPH_MAX_PASSES
      (type_state, Hashtbl.create 0)
  in
  Log.debug (fun m -> m "Body-inferred type fixpoint: %d outer passes, %d caller-arg-types"
    outer_iters (Hashtbl.length caller_arg_types));
  let type_state =
    Type_augment.build_module_singleton_types ~uses_new_keyword type_state file_infos
  in
  let default_export_class, named_export_classes =
    Type_augment.build_export_class_indexes ~lang ~type_state file_infos in

  (* CommonJS [module.exports = ...] default-export fn.  See [Cjs_exports]. *)
  let default_export_fn = Cjs_exports.build_default_export_fn ~lang file_infos in
  Log.debug (fun m -> m "default_export_fn: %d files"
    (Hashtbl.length default_export_fn));
  let file_funcs_index = Type_augment.build_file_funcs_index all_funcs in
  let path_suffix_index : (string, string list) Hashtbl.t option =
    if Lang.equal lang Lang.Ts || Lang.equal lang Lang.Js then
      Some (Ts_modules.build_path_suffix_index
              (List.map (fun fi -> Fpath.to_string fi.fi_file) file_infos))
    else None
  in
  Log.debug (fun m -> m "Exports: %d default / %d named.  File-funcs index: %d files.  Suffix index: %d entries"
    (Hashtbl.length default_export_class)
    (Hashtbl.length named_export_classes)
    (Hashtbl.length file_funcs_index)
    (match path_suffix_index with Some index -> Hashtbl.length index | None -> 0));

  (* Project-wide free-function indexes.  See [Func_index]. *)
  let project_funcs_by_package = Func_index.build_by_package ~cfg all_funcs in
  Log.debug (fun m -> m "Per-package func index: %d packages (Per_directory only)"
    (Hashtbl.length project_funcs_by_package));
  let project_funcs_by_module =
    Func_index.build_by_module ~cfg ~file_infos all_funcs
  in
  Log.debug (fun m -> m "Per-module func index: %d modules (Per_file only)"
    (Hashtbl.length project_funcs_by_module));

  (* Re-export pass for [`Per_file] languages.  See [Reexports]. *)
  if cfg.Index_lang_rules.unqualified_scope = `Per_file then
    Reexports.resolve_into_module_index
      ~project_funcs_by_module file_infos
    |> List.iter (fun (qn, funcs) ->
         Hashtbl.replace project_funcs_by_module qn funcs);

  (* Defining-file -> package module qn; disambiguates same-basename packages
     for method-homonym resolution. *)
  let file_module_qn : (string, Names.Module_qn.t) Hashtbl.t =
    let index = Hashtbl.create (List.length file_infos) in
    List.iter (fun (fi : file_info) ->
      Hashtbl.replace index (Fpath.to_string fi.fi_file) fi.fi_module_path
    ) file_infos;
    index
  in

  (* Directory and per-file visible-names sets.  See [Visibility]. *)
  let dir_visible_names =
    Visibility.build_dir_index ~cfg file_infos
  in
  let visible_names_for_file =
    Visibility.for_file ~cfg ~dir_visible_names ~project_funcs_by_module
  in

  (* Per-AST [extract_calls] into an edge list; graph mutated only in the
     merge step. *)
  let pipeline_ctx : Pipeline.ctx =
    { Pipeline.lang;
      cfg;
      type_state;
      all_funcs;
      project_funcs_by_name;
      project_funcs_by_module;
      file_module_qn;
      project_funcs_by_package;
      project_class_names;
      file_funcs_index;
      default_export_class;
      named_export_classes;
      default_export_fn;
      path_suffix_index;
      slice_element_of_field;
      top_level_node_for;
      visible_names_for_file;
      stamp_var_types =
        (fun ~type_state ~slice_element_of_field ast ->
          Type_augment.stamp_var_types_from_bodies ~uses_new_keyword
            ~type_state ~slice_element_of_field ast);
      resolve_ts_specifier =
        (fun ~path_suffix_index ~current_file specifier ->
          Ts_modules.resolve_specifier ~path_suffix_index ~current_file specifier);
    }
  in
  let edges_for_file fi = Pipeline.edges_for_file pipeline_ctx fi in
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
  let per_file_edges = run_per_file caps ~ncores edges_for_file file_infos in
  List.iter (function
    | Ok edges ->
      List.iter (fun (src, dst, call_tok) ->
        Call_graph.add_edge graph ~src ~dst ~call_tok)
        edges
    | Error msg ->
      Log.warn (fun m -> m "[skip] phase 2 worker failed: %s" msg))
    per_file_edges;
  (* Interface dispatch edges.  See [Structural_dispatch]. *)
  let n_dispatch =
    Structural_dispatch.emit_dispatch_edges
      ~cfg ~type_state ~func_def_file:Type_augment.func_def_file ~class_infos ~graph
  in
  if n_dispatch > 0 then
    Log.debug (fun m -> m "Interface dispatch: emitted %d Dispatch edges"
      n_dispatch);
  (graph, inherited_by_class)

let project_root_abs_of (project_root : Fpath.t) : Fpath.t =
  if Fpath.is_abs project_root then project_root
  else Fpath.(v (Sys.getcwd ()) // project_root) |> Fpath.normalize

let run_pipeline (caps : < Cap.fork >)
    ?(targeting_conf : Find_targets.conf =
                Discover.projidx_default_targeting_conf)
    ~(lang : Lang.t) ~(project_root : Fpath.t) ~(ncores : int)
    ~(includes : string list) ~(excludes : string list) ()
  : entry list * Call_graph.G.t * int * int * file_info list =
  let cfg = Index_lang_rules.for_lang lang in
  (* [discover_excludes] so the CLI and embedded engine index the same files. *)
  let excludes =
    excludes @ cfg.Index_lang_rules.discover_excludes ~project_root
  in
  let files = Discover.discover_files ~targeting_conf
    ~lang ~project_root ~includes ~excludes in
  let n_total = List.length files in
  Log.info (fun m -> m "Discovered %d %s files. Parsing with %d domain(s)..."
    n_total (Lang.to_string lang) ncores);
  (* Rust-only: rewrite [impl Foo {...}] ([OtherDef("Impl")]) into a [ClassDef]
     so the walkers see the methods. *)
  (* Rust only: impl methods must look like class methods to every later
     pass, so the STORED ast is reshaped too ([cfg.class_def_reshape]).
     Go/Ruby also wire the hook but reshape only the collector's view —
     their stored TypeDefs/ModuleDefs must survive for the embedding and
     module walks. *)
  let reshape_class_defs (ast : G.program) : G.program =
    if not (Lang.equal lang Lang.Rust) then ast
    else
      List.map (fun (stmt : G.stmt) ->
        match stmt.G.s with
        | G.DefStmt (ent, def_kind) ->
          (match cfg.Index_lang_rules.class_def_reshape ent def_kind with
           | Some (new_ent, new_kind) ->
             { stmt with G.s = G.DefStmt (new_ent, new_kind) }
           | None -> stmt)
        | _ -> stmt
      ) ast
  in
  (* Absolutize paths: interface dispatch's [family_key] needs consistent
     directory prefixes. *)
  let project_root_abs = project_root_abs_of project_root in
  let absolutize (file : Fpath.t) : Fpath.t =
    if Fpath.is_abs file then file
    else Fpath.(project_root_abs // file) |> Fpath.normalize
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
  let process file =
    let file = absolutize file in
    let ast =
      Parse_target.parse_and_resolve_name_warn_if_partial lang file
    in
    let ast = reshape_class_defs ast in
    let mp =
      match Go_modules.import_path_of_dir go_modules (Fpath.parent file) with
      | Some path -> Names.Module_qn.of_string path
      | None -> module_path ~cfg ~project_root ~ast file
    in
    Symbols.collect_in_ast ~cfg ~lang ~module_path:mp ~file ast
  in
  let results =
    if ncores <= 1 then
      List.map (fun file ->
        try Ok (process file)
        with
        | Out_of_memory | Stack_overflow | Time_limit.Timeout _ as exn ->
          raise exn
        | exn -> Error (file, Printexc.to_string exn)
      ) files
    else
      Domainslib_.parmap caps
        ~num_domains:ncores
        ~chunksize:1
        ~exception_handler:(fun file exc ->
          (match Exception.get_exn exc with
           | Out_of_memory | Stack_overflow | Time_limit.Timeout _ ->
             Exception.reraise exc
           | exn -> (file, Printexc.to_string exn)))
        process
        files
  in
  let scanned, skipped, all_entries, all_classes, all_files, _ =
    List.fold_left (fun (sc, sk, es, cs, fis, n_logged) -> function
      | Ok (entries, class_infos, fi) ->
        (sc + 1, sk,
         List.rev_append entries es,
         List.rev_append class_infos cs,
         fi :: fis,
         n_logged)
      | Error (file, msg) ->
        if n_logged < 5 then
          Log.warn (fun m -> m "[skip] %s: %s" (Fpath.to_string file) msg);
        (sc, sk + 1, es, cs, fis, n_logged + 1)
    ) (0, 0, [], [], [], 0) results
  in
  let reexport_map = Reexports.build_reexport_map ~cfg all_files in
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
    Symbols.dataclass_wrapper_synth_entries ~cfg ~wrappers all_entries all_classes
  in
  Log.debug (fun m -> m "Wrapper synthesis: %d dunders emitted"
    (List.length synth_from_wrappers));
  let entries_pre_mro = all_entries @ synth_from_wrappers in
  let graph, inherited_by_class =
    build_project_call_graph caps ~cfg ~lang ~ncores
      ~class_infos:all_classes ~reexport_map all_files
  in
  (* Inherited-method entry rows, derived from the same C3 linearisation
     callee resolution reads, so the diagnostic dump matches what resolution
     sees.  Only [collect] consumers use these rows; [collect_resolved]
     discards them.  The derivation is one pass over the C3 output; consider
     gating it to the [collect] path if it ever shows up in profiles. *)
  let inherited =
    List.concat_map (fun ((ci : class_info), funcs) ->
      List.filter_map (fun (func : Func_info.t) ->
        Option.map (fun (meth : IL.name) ->
          let method_name = fst meth.IL.ident in
          { id = Symbols.synth_function_id ci.ci_id method_name;
            name = method_name; kind = K_method;
            file = ci.ci_file; range = ci.ci_range;
            defining_class_id = Some ci.ci_id })
          (Func_info.leaf_name func.Func_info.fn_id))
        funcs)
      inherited_by_class
  in
  Log.debug (fun m -> m "Inheritance: %d inherited method entries (lang walks_inheritance=%b)"
    (List.length inherited) cfg.Index_lang_rules.walks_inheritance);
  let final_entries = entries_pre_mro @ inherited in
  Log.info (fun m -> m "Call graph: %d vertices, %d edges"
    (Call_graph.G.nb_vertex graph) (Call_graph.G.nb_edges graph));
  (final_entries, graph, scanned, skipped, all_files)

let collect (caps : < Cap.fork >)
    ?(targeting_conf : Find_targets.conf =
                Discover.projidx_default_targeting_conf)
    ~(lang : Lang.t) ~(project_root : Fpath.t) ~(ncores : int)
    ~(includes : string list) ~(excludes : string list) ()
  : entry list * Call_graph.G.t * int * int =
  let (entries, graph, scanned, skipped, _all_files) =
    run_pipeline caps ~targeting_conf ~lang ~project_root ~ncores
      ~includes ~excludes ()
  in
  (entries, graph, scanned, skipped)

let collect_resolved (caps : < Cap.fork >)
    ?(targeting_conf : Find_targets.conf =
                Discover.projidx_default_targeting_conf)
    ~(lang : Lang.t) ~(project_root : Fpath.t) ~(ncores : int)
    ~(includes : string list) ~(excludes : string list) ()
  : Call_graph.G.t * (string, G.program) Hashtbl.t =
  let project_root_abs = project_root_abs_of project_root in
  let absnorm (file : Fpath.t) : string =
    (if Fpath.is_abs file then file else Fpath.(project_root_abs // file))
    |> Fpath.normalize |> Fpath.to_string
  in
  let (_entries, graph, _scanned, _skipped, all_files) =
    run_pipeline caps ~targeting_conf ~lang ~project_root:project_root_abs
      ~ncores ~includes ~excludes ()
  in
  let tbl = Hashtbl.create (List.length all_files) in
  List.iter (fun (fi : file_info) ->
    Hashtbl.replace tbl (absnorm fi.fi_file) fi.fi_ast)
    all_files;
  (graph, tbl)

let resolve_ast_for_file (caps : < Cap.fork >)
    ?(targeting_conf : Find_targets.conf =
                Discover.projidx_default_targeting_conf)
    ~(lang : Lang.t) ~(project_root : Fpath.t) ~(ncores : int)
    ~(target : Fpath.t) ()
  : G.program option =
  let project_root_abs = project_root_abs_of project_root in
  let target_key =
    (if Fpath.is_abs target then target else Fpath.(project_root_abs // target))
    |> Fpath.normalize |> Fpath.to_string
  in
  let _graph, asts =
    collect_resolved caps ~targeting_conf ~lang ~project_root ~ncores
      ~includes:[] ~excludes:[] ()
  in
  Hashtbl.find_opt asts target_key
