module FA = Graph_from_AST
module G = AST_generic
open Types


(* Infer var classes from assignments and stamp them onto [id_type]. *)
type stamp_var_types =
  type_state:Type_state.t ->
  slice_element_of_field:(string * string, G.name) Hashtbl.t ->
  G.program ->
  unit

type ctx = {
  lang : Lang.t;
  cfg : Index_lang_rules.t;
  type_state : Type_state.t;
  all_funcs : Func_info.t list;
  project_funcs_by_name : (string, Func_info.t list) Hashtbl.t;
  project_funcs_by_module :
    (Names.Module_qn.t, Func_info.t list) Hashtbl.t;
  file_module_qn : (string, Names.Module_qn.t) Hashtbl.t;
  project_funcs_by_package : (string, Func_info.t list) Hashtbl.t;
  project_class_names : G.name list;
  file_funcs_index : (string, Func_info.t list) Hashtbl.t;
  default_export_class : (string, G.name) Hashtbl.t;
  named_export_classes : (string * string, G.name) Hashtbl.t;
  default_export_fn : (string, Func_info.t) Hashtbl.t;
  path_suffix_index : (string, string list) Hashtbl.t option;
  slice_element_of_field : (string * string, G.name) Hashtbl.t;
  top_level_node_for : Fpath.t -> Function_id.t;
  visible_names_for_file : file_info -> (string, unit) Hashtbl.t;
  stamp_var_types : stamp_var_types;
  resolve_ts_specifier :
    path_suffix_index:(string, string list) Hashtbl.t option ->
    current_file:Fpath.t -> string -> string list;
}

(* Detect ctor/import-derived var classes and stamp them onto [id_type].
   Side effect on [visible]: extends it with discovered class names so
   [build_funcs_by_name] keeps their methods. *)
let stamp_base_var_types
    ~(lang : Lang.t)
    ~(project_class_names : G.name list)
    ~(default_export_class : (string, G.name) Hashtbl.t)
    ~(named_export_classes : (string * string, G.name) Hashtbl.t)
    ~(path_suffix_index : (string, string list) Hashtbl.t option)
    ~(resolve_ts_specifier :
        path_suffix_index:(string, string list) Hashtbl.t option ->
        current_file:Fpath.t -> string -> string list)
    ~(visible : (string, unit) Hashtbl.t)
    (fi : file_info) : unit =
  let ctor_facts =
    Object_initialization.detect_object_initialization
      ~extra_class_names:project_class_names fi.fi_ast lang
  in
  let import_facts =
    List.fold_left (fun acc (local, specifier, kind) ->
      let candidates =
        resolve_ts_specifier ~path_suffix_index ~current_file:fi.fi_file specifier
      in
      let cls_opt =
        List.find_map (fun path ->
          match kind with
          | I_default -> Hashtbl.find_opt default_export_class path
          | I_named name ->
            (match Hashtbl.find_opt named_export_classes (path, name) with
             | Some _ as found -> found
             | None -> Hashtbl.find_opt default_export_class path)
          | I_namespace ->
            Hashtbl.find_opt default_export_class path
        ) candidates
      in
      match cls_opt with
      | Some cls ->
        let local_id = G.Id ((local, Tok.unsafe_fake_tok local),
                             G.empty_id_info ()) in
        (local_id, cls) :: acc
      | None -> acc
    ) [] fi.fi_import_specifiers
  in
  let facts = import_facts @ ctor_facts in
  List.iter (fun (_var, class_name) ->
    match class_name with
    | G.Id ((name_str, _), _) -> Hashtbl.replace visible name_str ()
    | _ -> ()
  ) facts;
  Object_initialization.stamp_id_types facts fi.fi_ast

let build_alias_to_module_qn
    ~(cfg : Index_lang_rules.t) (fi : file_info)
  : (string, Names.Module_qn.t) Hashtbl.t option =
  match cfg.Index_lang_rules.unqualified_scope with
  | `Per_file | `Per_directory ->
    let tbl : (string, Names.Module_qn.t) Hashtbl.t = Hashtbl.create 16 in
    List.iter (fun (local, target_qn) ->
      if String.length local > 0
         && not (Names.Module_qn.is_empty target_qn) then begin
        let first_seg =
          match Names.Module_qn.parts target_qn with
          | [] -> Names.Module_qn.to_string target_qn
          | first_part :: _ -> first_part
        in
        let bound_qn =
          if String.equal local first_seg
          then Names.Module_qn.of_string local
          else target_qn
        in
        Hashtbl.replace tbl local bound_qn
      end
    ) fi.fi_imports;
    if Int.equal (Hashtbl.length tbl) 0 then None else Some tbl
  | _ -> None

let build_file_funcs_by_package
    ~(cfg : Index_lang_rules.t)
    ~(project_funcs_by_package : (string, Func_info.t list) Hashtbl.t)
    ~(project_funcs_by_module :
        (Names.Module_qn.t, Func_info.t list) Hashtbl.t)
    ~(file_funcs_index : (string, Func_info.t list) Hashtbl.t)
    ~(path_suffix_index : (string, string list) Hashtbl.t option)
    ~(resolve_ts_specifier :
        path_suffix_index:(string, string list) Hashtbl.t option ->
        current_file:Fpath.t -> string -> string list)
    (fi : file_info)
  : (string, Func_info.t list) Hashtbl.t option =
  if cfg.Index_lang_rules.unqualified_scope = `Per_directory then begin
    let alias_extra = List.filter_map (fun (local, target) ->
      let target_str = Names.Module_qn.to_string target in
      let basename = Filename.basename target_str in
      if local = basename then None
      else
        match Hashtbl.find_opt project_funcs_by_package basename with
        | Some fs -> Some (local, fs)
        | None -> None
    ) fi.fi_imports in
    if alias_extra = [] then Some project_funcs_by_package
    else begin
      let tbl = Hashtbl.copy project_funcs_by_package in
      List.iter (fun (key, funcs) -> Hashtbl.replace tbl key funcs) alias_extra;
      Some tbl
    end
  end
  else begin
    let alias_extra_ts = List.filter_map (fun (local, specifier, _kind) ->
      let candidates =
        resolve_ts_specifier ~path_suffix_index ~current_file:fi.fi_file specifier
      in
      let funcs =
        List.concat_map (fun path ->
          Option.value (Hashtbl.find_opt file_funcs_index path) ~default:[]
        ) candidates
      in
      if funcs = [] then None else Some (local, funcs)
    ) fi.fi_import_specifiers in
    let alias_extra_py = List.filter_map (fun (local, target_qn) ->
      match Hashtbl.find_opt project_funcs_by_module target_qn with
      | Some fs -> Some (local, fs)
      | None -> None
    ) fi.fi_imports in
    let alias_extra = alias_extra_ts @ alias_extra_py in
    if alias_extra = [] then Some project_funcs_by_package
    else begin
      let tbl = Hashtbl.copy project_funcs_by_package in
      List.iter (fun (key, funcs) -> Hashtbl.replace tbl key funcs) alias_extra;
      Some tbl
    end
  end

let build_import_target_files
    ~(path_suffix_index : (string, string list) Hashtbl.t option)
    ~(resolve_ts_specifier :
        path_suffix_index:(string, string list) Hashtbl.t option ->
        current_file:Fpath.t -> string -> string list)
    (fi : file_info)
  : (string, (string, unit) Hashtbl.t) Hashtbl.t =
  let target_files = Hashtbl.create 64 in
  List.iter (fun (local, specifier, _kind) ->
    let candidates =
      resolve_ts_specifier ~path_suffix_index ~current_file:fi.fi_file specifier
    in
    if candidates <> [] then begin
      let set =
        match Hashtbl.find_opt target_files local with
        | Some file_set -> file_set
        | None ->
          let file_set = Hashtbl.create 4 in
          Hashtbl.replace target_files local file_set; file_set
      in
      List.iter (fun path -> Hashtbl.replace set path ()) candidates
    end
  ) fi.fi_import_specifiers;
  target_files

let build_same_file_funcs_by_name
    ~(file_funcs_index : (string, Func_info.t list) Hashtbl.t)
    ~(fi_file_str : string)
  : (string, Func_info.t list) Hashtbl.t =
  let tbl = Hashtbl.create 64 in
  let same_file_list =
    Option.value (Hashtbl.find_opt file_funcs_index fi_file_str)
      ~default:[]
  in
  List.iter (fun (func : Func_info.t) ->
    match List_.init_and_last_opt func.Func_info.fn_id with
    | Some (_, Some leaf) ->
      let name = fst leaf.IL.ident in
      let cur = Option.value (Hashtbl.find_opt tbl name) ~default:[] in
      Hashtbl.replace tbl name (func :: cur)
    | _ -> ()
  ) same_file_list;
  tbl

let build_funcs_by_name
    ~(visible : (string, unit) Hashtbl.t)
    ~(project_funcs_by_name : (string, Func_info.t list) Hashtbl.t)
    ~(default_export_fn : (string, Func_info.t) Hashtbl.t)
    ~(path_suffix_index : (string, string list) Hashtbl.t option)
    ~(resolve_ts_specifier :
        path_suffix_index:(string, string list) Hashtbl.t option ->
        current_file:Fpath.t -> string -> string list)
    ~(import_target_files :
        (string, (string, unit) Hashtbl.t) Hashtbl.t)
    ~(func_file_opt : Func_info.t -> string option)
    ~(func_in_caller_file : Func_info.t -> bool)
    (fi : file_info)
  : (string, Func_info.t list) Hashtbl.t option =
  let tbl = Hashtbl.create (Hashtbl.length visible) in
  Hashtbl.iter (fun name () ->
    match Hashtbl.find_opt project_funcs_by_name name with
    | None -> ()
    | Some fs ->
      let kept = List.filter (fun (func : Func_info.t) ->
        match Func_info.as_method func.Func_info.fn_id with
        | Some (cls, _) -> Hashtbl.mem visible (fst cls.IL.ident)
        | None -> true
      ) fs in
      let kept =
        match Hashtbl.find_opt import_target_files name with
        | None -> kept
        | Some target_set ->
          let matches = List.filter (fun func ->
            func_in_caller_file func
            || (match func_file_opt func with
                | Some file_str -> Hashtbl.mem target_set file_str
                | None -> false)
          ) kept in
          if matches <> [] then matches else kept
      in
      let same, other = List.partition func_in_caller_file kept in
      let kept = same @ other in
      if kept <> [] then Hashtbl.replace tbl name kept
  ) visible;
  List.iter (fun (local, specifier, kind) ->
    match kind with
    | I_default ->
      let candidates =
        resolve_ts_specifier ~path_suffix_index ~current_file:fi.fi_file specifier
      in
      List.iter (fun path ->
        match Hashtbl.find_opt default_export_fn path with
        | None -> ()
        | Some target ->
          (* Expose the target under the importer's local name at the
             TARGET's identity: the synth leaf carries [local] at the
             target's position/sid, so name lookup finds [local] while
             fn_id_to_node / resolved_name_of_fn_id resolve to the
             target's real vertex (where its body and signature live) —
             the same same-position/different-name convention as
             Ts_class_aliases and Reexports.expose_free_as. A lambda
             default export (synthetic [_module_exports_default], no real
             vertex) stays unresolved, as before. *)
          (match Func_info.leaf_name target.Func_info.fn_id with
           | None -> ()
           | Some (tname : IL.name) ->
             let alias_ii = G.empty_id_info () in
             alias_ii.G.id_resolved :=
               (match !(tname.IL.id_info.G.id_resolved) with
                | Some _ as r -> r
                | None -> Some (G.Global, tname.IL.sid));
             let il_name = IL.{
               ident = (local, snd tname.IL.ident);
               sid = tname.IL.sid;
               id_info = alias_ii;
             } in
             let synth : Func_info.t = {
               fn_id = [None; Some il_name];
               entity = target.Func_info.entity;
               fdef = target.Func_info.fdef;
             } in
             let cur = Option.value (Hashtbl.find_opt tbl local) ~default:[] in
             Hashtbl.replace tbl local (synth :: cur))
      ) candidates
    | _ -> ()
  ) fi.fi_import_specifiers;
  Some tbl

(* Imported module singletons: stamp [local]'s occurrences with the
   singleton's class. *)
let stamp_singleton_imports
    ~(type_state : Type_state.t)
    (fi : file_info) : unit =
  let facts =
    List.fold_left (fun acc (local, target_qn) ->
      if String.length local = 0 then acc
      else
        match Type_state.get_module_singleton type_state target_qn with
        | None -> acc
        | Some ty ->
          let v_id =
            G.Id ((local, Tok.unsafe_fake_tok local), G.empty_id_info ())
          in
          (v_id, ty) :: acc
    ) [] fi.fi_imports
  in
  Object_initialization.stamp_id_types facts fi.fi_ast

let edges_for_file (ctx : ctx) (fi : file_info)
  : (Function_id.t * Function_id.t * Tok.t) list =
  let { lang; cfg; type_state; all_funcs;
        project_funcs_by_name; project_funcs_by_module; file_module_qn;
        project_funcs_by_package; project_class_names;
        file_funcs_index;
        default_export_class; named_export_classes; default_export_fn;
        path_suffix_index; slice_element_of_field;
        top_level_node_for; visible_names_for_file;
        stamp_var_types; resolve_ts_specifier } = ctx in
  let skip_anon (opt_ent : G.entity option) =
    not cfg.Index_lang_rules.include_anonymous_funcs && Option.is_none opt_ent
  in
    let emitter =
      Edge_emitter.create ~top_level:(top_level_node_for fi.fi_file)
    in
    let visible = visible_names_for_file fi in
    let fi_file_str = Fpath.to_string fi.fi_file in
    let top_level_node = top_level_node_for fi.fi_file in
    let func_file_opt (func : FA.func_info) : string option =
      Option.map Fpath.to_string (Func_info.def_file_opt func)
    in
    let func_in_caller_file (func : FA.func_info) : bool =
      match func_file_opt func with
      | Some file_str -> file_str = fi_file_str
      | None -> false
    in
    (* Must run before [build_funcs_by_name]: augments [visible] with cross-file class targets it filters on. *)
    stamp_base_var_types ~lang ~project_class_names
      ~default_export_class ~named_export_classes
      ~path_suffix_index ~resolve_ts_specifier ~visible fi;
    let alias_to_module_qn = build_alias_to_module_qn ~cfg fi in
    let funcs_by_module_qn
      : (Names.Module_qn.t, FA.func_info list) Hashtbl.t option =
      match cfg.Index_lang_rules.unqualified_scope with
      | `Per_file | `Per_directory -> Some project_funcs_by_module
      | _ -> None
    in
    let file_funcs_by_package =
      build_file_funcs_by_package ~cfg ~project_funcs_by_package
        ~project_funcs_by_module ~file_funcs_index
        ~path_suffix_index ~resolve_ts_specifier fi
    in
    let file_type_state =
      cfg.Index_lang_rules.narrow_methods_by_imports
        ~fi_imports:fi.fi_imports ~file_of_func:func_file_opt type_state
    in
    let import_target_files =
      build_import_target_files ~path_suffix_index ~resolve_ts_specifier fi
    in
    let same_file_funcs_by_name =
      build_same_file_funcs_by_name ~file_funcs_index ~fi_file_str
    in
    let funcs_by_name =
      build_funcs_by_name ~visible ~project_funcs_by_name ~default_export_fn
        ~path_suffix_index ~resolve_ts_specifier ~import_target_files
        ~func_file_opt ~func_in_caller_file fi
    in
    let func_lookup =
      Func_lookup.create
        ?funcs_by_name:(Option.map Func_lookup.leaf_index_of_hashtbl funcs_by_name)
        ~project_funcs_by_name:
          (Func_lookup.leaf_index_of_hashtbl project_funcs_by_name)
        ?funcs_by_module_qn:
          (Option.map Func_lookup.module_index_of_hashtbl funcs_by_module_qn)
        ?alias_to_module_qn:
          (Option.map Func_lookup.alias_index_of_hashtbl alias_to_module_qn)
        ~same_file_funcs_by_name:
          (Func_lookup.leaf_index_of_hashtbl same_file_funcs_by_name)
        ?funcs_by_package:
          (Option.map Func_lookup.leaf_index_of_hashtbl file_funcs_by_package)
        ~file_module_qn:
          (Func_lookup.file_module_index_of_hashtbl file_module_qn)
        ()
    in
    stamp_singleton_imports ~type_state fi;
    stamp_var_types ~type_state ~slice_element_of_field fi.fi_ast;
    let per_fdef_edges =
      Visit_function_defs.fold_with_parent_path ~lang
        (fun edges opt_ent parent_path fdef ->
        (* A skipped anon attributes calls to its enclosing named ancestor, else the file's [<top_level>] node. *)
        let caller_kind =
          if skip_anon opt_ent then begin
            match parent_path with
            | [] | [None] -> `Top_anon
            | _ -> `Named parent_path
          end else
            match FA.fn_id_of_entity ~lang opt_ent parent_path fdef with
            | Some fn_id -> `Named fn_id
            | None -> `Skip
        in
        match caller_kind with
        | `Skip -> edges
        | _ ->
          let fn_id_for_extract : FA.fn_id =
            match caller_kind with `Named fn_id -> fn_id | _ -> []
          in
          let caller_node_opt : Call_graph.G.V.t option =
            match caller_kind with
            | `Named fn_id -> FA.fn_id_to_node fn_id
            | `Top_anon -> Some top_level_node
            | `Skip -> None
          in
          let is_toplevel_lambda =
            match (opt_ent, parent_path) with
            | (None, [None]) | (None, []) -> true
            | _ -> false
          in
          let fn_id = fn_id_for_extract in
          let body_stmt = AST_generic_helpers.funcbody_to_stmt fdef.G.fbody in
          let body_program = [body_stmt] in
          (* fdef-scoped facts stamped onto the body only, most specific
             first (fill-on-None): isinstance narrowing, then [self]/[cls],
             then typed params. *)
          let fdef_facts =
            let param_facts =
              Tok.unbracket fdef.G.fparams
              |> List.filter_map (fun param ->
                match param with
                | G.ParamReceiver { G.pname = Some pn; ptype = Some pty; _ }
                | G.Param { G.pname = Some pn; ptype = Some pty; _ } ->
                  (match Ty_leaf.inner_class_name_of_ty pty with
                   | Some cls -> Some (G.Id (pn, G.empty_id_info ()), cls)
                   | None -> None)
                | _ -> None)
            in
            (* An anon fdef's own [fparams] lacks [self]/[cls]; bind them to the enclosing class. *)
            let self_facts =
              match opt_ent, parent_path with
              | None, (Some (cls : IL.name)) :: _
                when Type_state.has_class file_type_state (fst cls.IL.ident) ->
                let cls_id = G.Id (cls.IL.ident, G.empty_id_info ()) in
                let mk str =
                  G.Id ((str, Tok.unsafe_fake_tok str), G.empty_id_info ())
                in
                [ (mk "self", cls_id); (mk "cls", cls_id) ]
              | _ -> []
            in
            (* isinstance narrowing over-applies: [(var, T)] holds for the
               whole function, not just the narrowed scope. *)
            let isinstance_facts =
              if not (Lang.equal lang Lang.Python
                   || Lang.equal lang Lang.Python2
                   || Lang.equal lang Lang.Python3) then []
              else
                Walker.fold_exprs_in_stmt ~skip_nested_fdefs:true
                  (fun acc expr ->
                    match expr.G.e with
                    | G.Call ({ e = G.N (G.Id (("isinstance", _), _)); _ },
                              (_, [G.Arg var_e; G.Arg ty_e], _)) ->
                      (match var_e.G.e, ty_e.G.e with
                       | G.N (G.Id _ as var_n), G.N (G.Id _ as ty_n)
                       | G.N (G.Id _ as var_n), G.N (G.IdQualified _ as ty_n) ->
                         let ty_leaf = match ty_n with
                           | G.Id _ -> ty_n
                           | G.IdQualified { name_last = ((str, tok), _); _ } ->
                             G.Id ((str, tok), G.empty_id_info ())
                         in
                         (var_n, ty_leaf) :: acc
                       | _ -> acc)
                    | _ -> acc) [] body_stmt
            in
            isinstance_facts @ self_facts @ param_facts
          in
          Object_initialization.stamp_id_types fdef_facts body_program;
          stamp_var_types ~type_state ~slice_element_of_field body_program;
          let { FA.calls = callee_calls; callbacks = callback_calls; _ } =
            FA.extract_calls ~lang ~all_funcs
              ~func_lookup ~type_state:file_type_state
              ~caller_parent_path:fn_id fdef
          in
          let edges =
            List.fold_left (fun edges (callee, call_tok) ->
              Edge_emitter.emit_call emitter ~caller_node:caller_node_opt
                ~is_toplevel_lambda ~callee ~call_tok @ edges
            ) edges callee_calls
          in
          let edges =
            List.fold_left (fun edges (callback, call_tok, tmp) ->
              Edge_emitter.emit_callback emitter ~caller_node:caller_node_opt
                ~is_toplevel_lambda ~callback ~call_tok ~tmp @ edges
            ) edges callback_calls
          in
          (match opt_ent with
           | None -> edges
           | Some ent when ent.G.attrs = [] -> edges
           | Some ent ->
             let dec_calls =
               FA.extract_decorator_calls ~lang
                 ~all_funcs
                 ~func_lookup ~type_state:file_type_state
                 ~caller_parent_path:fn_id ent.G.attrs
             in
             List.fold_left (fun edges (callee, call_tok) ->
               Edge_emitter.emit_call emitter ~caller_node:caller_node_opt
                 ~is_toplevel_lambda ~callee ~call_tok @ edges
             ) edges dec_calls))
      [] fi.fi_ast
    in
    let toplevel_calls =
      FA.extract_toplevel_calls ~lang ~all_funcs
        ~func_lookup ~type_state:file_type_state
        fi.fi_ast
    in
    let toplevel_call_edges =
      List.fold_left (fun edges (callee, call_tok) ->
        Edge_emitter.emit_toplevel emitter ~callee ~call_tok @ edges
      ) [] toplevel_calls
    in
    let toplevel_callbacks =
      let merged_funcs_by_name =
        match funcs_by_name with
        | None -> project_funcs_by_name
        | Some pf ->
          let merged = Hashtbl.copy project_funcs_by_name in
          Hashtbl.iter (fun name fs ->
            let cur = Option.value (Hashtbl.find_opt merged name) ~default:[] in
            Hashtbl.replace merged name (fs @ cur)
          ) pf;
          merged
      in
      let toplevel_func_lookup =
        Func_lookup.create
          ~funcs_by_name:(Func_lookup.leaf_index_of_hashtbl merged_funcs_by_name)
          ()
      in
      FA.extract_toplevel_hof_callbacks ~lang ~all_funcs
        ~func_lookup:toplevel_func_lookup fi.fi_ast
    in
    let toplevel_callback_edges =
      List.fold_left (fun edges (callback, call_tok) ->
        Edge_emitter.emit_toplevel emitter ~callee:callback ~call_tok @ edges
      ) [] toplevel_callbacks
    in
    per_fdef_edges @ toplevel_call_edges @ toplevel_callback_edges
