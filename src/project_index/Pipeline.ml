module FA = Graph_from_AST
module G = AST_generic
open Types

(* Wall-clock time of the stages of [edges_for_file], summed across every
   file and every domain; [Project_index] logs it after the edge pass. *)
let edge_stage_secs : (string, float) Hashtbl.t = Hashtbl.create 8
let edge_stage_mutex = Mutex.create ()

let staged (name : string) (f : unit -> 'a) : 'a =
  let res, secs = Common.with_time f in
  Mutex.lock edge_stage_mutex;
  let prev = Option.value (Hashtbl.find_opt edge_stage_secs name) ~default:0. in
  Hashtbl.replace edge_stage_secs name (prev +. secs);
  Mutex.unlock edge_stage_mutex;
  res

let edge_stage_report () : (string * float) list =
  Hashtbl.fold (fun k v acc -> (k, v) :: acc) edge_stage_secs []
  |> List.sort (fun (_, a) (_, b) -> compare b a)


(* Infer var classes from assignments and stamp them onto
   [id_instance_type]. *)
type stamp_var_types =
  type_state:Type_state.t ->
  slice_element_of_field:(string * string, G.name) Hashtbl.t ->
  G.program ->
  unit

type required_files_narrowing = {
  narrowable_classes : Names.Class_name.t list;
  rev_path_segs_by_file : string list Common.SMap.t;
}

type file_scope = {
  scope_table : Func_lookup.scope_table;
  bound_class_files : (Names.Class_name.t * Fpath.t) list;
  own_modules : Names.Module_qn.t list;
  module_aliases : (string, Names.Module_qn.t) Hashtbl.t option;
}

type ctx = {
  lang : Lang.t;
  cfg : Index_lang_rules.t;
  type_state : Type_state.t;
  required_files_narrowing : required_files_narrowing option;
  definitions_by_qn : definition Common.SMap.t;
  attributes_by_module : Func_lookup.module_attributes;
  dunder_all : (string, unit) Hashtbl.t Common.SMap.t;
  resolution_orders : Func_lookup.resolution_orders;
  class_qn_by_definition : Func_lookup.class_qn_by_definition;
  methods_by_class : Func_lookup.methods_by_class;
  extensions_by_module : Func_info.t list Common.SMap.t Common.SMap.t;
  nested_types_by_class : Names.Class_qn.t Common.SMap.t Common.SMap.t;
  php_region_bindings : Scope_php.region_bindings Common.SMap.t;
  php_global_bindings : Scope_binding.positioned_binding list;
  module_scope : Scope_module.project_scope;
  classes_by_file : class_info list Common.SMap.t;
  class_parent_paths : (Function_id.t * IL.name option list) list Common.SMap.t;
  global_imports : import list;
  project_constructors : Func_lookup.constructor_index;
  project_funcs_by_name : (string, Func_info.t list) Hashtbl.t;
  project_funcs_by_module :
    (Names.Module_qn.t, Func_info.t list) Hashtbl.t;
  file_module_qn : (string, Names.Module_qn.t) Hashtbl.t;
  project_funcs_by_package : (string, Func_info.t list) Hashtbl.t;
  project_class_names : G.name list;
  file_funcs_index : (string, Func_info.t list) Hashtbl.t;
  slice_element_of_field : (string * string, G.name) Hashtbl.t;
  top_level_node_for : Fpath.t -> Function_id.t;
  visible_names_for_file : file_info -> (string, unit) Hashtbl.t;
  stamp_var_types : stamp_var_types;
  (* (module qn string, exported name) -> module-level bare-name alias
     value, for import-value svalue stamping. See
     [build_value_alias_index]. *)
  value_alias_index : (string * string, G.expr) Hashtbl.t;
}

(* Module-level bare-name value aliases ([f = sink] / [const f = sink] at
   the top level of a module): (module qn string, exported name) -> the
   aliased value expression. A name assigned more than once, or to
   anything but a bare non-local name, is dropped — the alias must be a
   static fact for the stamp to be sound. Feeds
   [stamp_import_value_aliases] (issue #499, cross-file alias). *)
let build_value_alias_index (file_infos : file_info list)
  : (string * string, G.expr) Hashtbl.t =
  let acceptable (e : G.expr) : G.expr option =
    match e.G.e with
    | G.N (G.Id (_, ainfo)) -> (
        match !(ainfo.G.id_resolved) with
        | Some ((G.LocalVar | G.Parameter | G.EnclosedVar), _) -> None
        | Some _
        | None -> Some e)
    | _ -> None
  in
  (* [None] = conflicting/unacceptable: poisoned, never stamped. *)
  let tbl : (string * string, G.expr option) Hashtbl.t = Hashtbl.create 16 in
  let record key rhs =
    match Hashtbl.find_opt tbl key with
    | Some _ -> Hashtbl.replace tbl key None
    | None -> Hashtbl.replace tbl key (acceptable rhs)
  in
  List.iter (fun (fi : file_info) ->
    let module_str = Names.Module_qn.to_string fi.fi_module_path in
    (* Module level only: the program's top-level statements. *)
    List.iter (fun (stmt : G.stmt) ->
      match stmt.G.s with
      | G.ExprStmt
          ({ e = G.Assign ({ e = G.N (G.Id ((name, _), _)); _ }, _, rhs);
             _ }, _) ->
        record (module_str, name) rhs
      | G.DefStmt
          ({ G.name = G.EN (G.Id ((name, _), _)); _ },
           G.VarDef { G.vinit = Some rhs; _ }) ->
        record (module_str, name) rhs
      | _ -> ())
      fi.fi_ast)
    file_infos;
  let out = Hashtbl.create 16 in
  Hashtbl.iter (fun k v ->
    match v with
    | Some e -> Hashtbl.replace out k e
    | None -> ()) tbl;
  out

(* Issue #499, cross-file alias: [xlib.py: f = sink] gives [f] a [Sym]
   svalue in xlib itself via naming, but an importing file's [f] is a
   fresh binding naming knows nothing about. Stamp the importing file's
   uses of a from-imported name whose target module exports it as a
   module-level bare-name alias; matching with [symbolic_propagation]
   then sees through the import exactly as through a local alias. Only
   empty svalue slots on [ImportedEntity]-resolved ids are written, so
   shadowing locals are untouched. *)
let stamp_import_value_aliases
    ~(value_alias_index : (string * string, G.expr) Hashtbl.t)
    (fi : file_info) : unit =
  if Hashtbl.length value_alias_index = 0 then ()
  else begin
    let by_local : (string, G.expr) Hashtbl.t = Hashtbl.create 4 in
    List.iter (fun (imp : import) ->
      let local = imp.im_local in
      (* From-imports record the full entity path (module ++ name). *)
      match List.rev (Names.Module_qn.parts imp.im_target) with
      | name :: (_ :: _ as rev_module) -> (
          let module_str =
            Names.Module_qn.of_parts (List.rev rev_module)
            |> Names.Module_qn.to_string
          in
          match Hashtbl.find_opt value_alias_index (module_str, name) with
          | Some value when not (Hashtbl.mem by_local local) ->
            Hashtbl.replace by_local local value
          | _ -> ())
      | _ -> ())
      fi.fi_imports;
    if Hashtbl.length by_local > 0 then begin
      let visitor =
        object
          inherit [_] G.iter as super

          method! visit_expr () e =
            (match e.G.e with
            | G.N (G.Id ((s, _), info)) -> (
                match
                  (Hashtbl.find_opt by_local s,
                   !(info.G.id_resolved),
                   !(info.G.id_svalue))
                with
                | Some value, Some (G.ImportedEntity _, _), None ->
                  info.G.id_svalue := Some (G.Sym value)
                | _ -> ())
            | _ -> ());
            super#visit_expr () e
        end
      in
      visitor#visit_program () fi.fi_ast
    end
  end

(* This pass reads the class of a variable from the constructor the
   variable is initialised with and stamps that class onto
   [id_instance_type].
   Side effect on [visible]: extends it with discovered class names so
   [build_funcs_by_name] keeps their methods. *)
let stamp_base_var_types
    ~(lang : Lang.t)
    ~(project_class_names : G.name list)
    ~(visible : (string, unit) Hashtbl.t)
    (fi : file_info) : unit =
  let facts =
    Object_initialization.detect_object_initialization
      ~extra_class_names:project_class_names fi.fi_ast lang
  in
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
  | `Per_module -> None
  | `Per_file | `Per_directory | `Per_namespace ->
    let tbl : (string, Names.Module_qn.t) Hashtbl.t = Hashtbl.create 16 in
    List.iter (fun (imp : import) ->
      let local = imp.im_local in
      let target_qn = imp.im_target in
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
    (fi : file_info)
  : Func_lookup.bare_name_index option =
  (* Import aliases in a per-file table; the shared project table is read
     only. *)
  let over_project (alias_extra : (string * Func_info.t list) list) =
    let aliases = Hashtbl.create (List.length alias_extra) in
    List.iter (fun (key, funcs) -> Hashtbl.replace aliases key funcs) alias_extra;
    Func_lookup.bare_name_index_override
      ~front:(Func_lookup.bare_name_index_of_hashtbl aliases)
      ~back:(Func_lookup.bare_name_index_of_hashtbl project_funcs_by_package)
  in
  let alias_extra =
    match cfg.Index_lang_rules.unqualified_scope with
    | `Per_directory ->
      List.filter_map (fun (imp : import) ->
        let target_str = Names.Module_qn.to_string imp.im_target in
        let basename = Filename.basename target_str in
        if String.equal imp.im_local basename then None
        else
          match Hashtbl.find_opt project_funcs_by_package basename with
          | Some fs -> Some (imp.im_local, fs)
          | None -> None
      ) fi.fi_imports
    | `Per_module -> []
    | `Per_file | `Per_package | `Per_namespace ->
      List.filter_map (fun (imp : import) ->
        match Hashtbl.find_opt project_funcs_by_module imp.im_target with
        | Some fs -> Some (imp.im_local, fs)
        | None -> None
      ) fi.fi_imports
  in
  match alias_extra with
  | [] -> Some (Func_lookup.bare_name_index_of_hashtbl project_funcs_by_package)
  | _ :: _ -> Some (over_project alias_extra)

(* Restrict colliding methods to files the caller itself requires (whole-file
   "*" import specifiers, Ruby [require_relative]) or the caller's own file.
   These languages bind no local name per import, so the required-file set
   applies to every class rather than to one imported name.  A spec matches a def file by trailing path segments, extensions
   stripped on the final segment of both sides ("widget_b" and "widget_b.php"
   both match ".../widget_b.rb"); leading "."/".." segments of a relative
   spec are dropped rather than resolved.  Callers with no whole-file requires
   (e.g. autoloaded Rails code) leave every group untouched. *)
let narrow_methods_by_required_files
    ~(required_specs : string list)
    ~(file_of_func : Func_info.t -> string option)
    ~(caller_file : string)
    ~(narrowing : required_files_narrowing)
    (ts : Type_state.t) : Type_state.t =
  let spec_suffixes =
    List.filter_map (fun spec ->
      match
        Path_segs.rev_no_ext spec
        |> List.filter (fun seg ->
             not (String.equal seg "") && not (String.equal seg ".")
             && not (String.equal seg ".."))
      with
      | [] -> None
      | segs -> Some segs)
      required_specs
  in
  if spec_suffixes = [] then ts
  else
    let keep_file (_ : Names.Class_name.t) (file : string) : bool =
      String.equal file caller_file
      || (let rev_file_segs =
            Common.SMap.find file narrowing.rev_path_segs_by_file
          in
          List.exists
            (fun rev_spec -> Path_segs.is_prefix rev_spec rev_file_segs)
            spec_suffixes)
    in
    Type_state.narrow ~classes:narrowing.narrowable_classes ~keep_file
      ~file_of_func ts

let resolves_by_binding (lang : Lang.t) : bool =
  match lang with
  | Lang.Python | Lang.Python2 | Lang.Python3
  | Lang.Java | Lang.Kotlin | Lang.Csharp | Lang.Php
  | Lang.Js | Lang.Ts -> true
  | _ -> false

let definition_of_target
    ~(definitions_by_qn : definition Common.SMap.t)
    ~(package : Names.Module_qn.t)
    (target : Names.Module_qn.t) : definition option =
  let stored_at (key : string) : definition option =
    Common.SMap.find_opt key definitions_by_qn
  in
  match stored_at (Names.Module_qn.to_string target) with
  | Some _ as found -> found
  | None ->
    if Names.Module_qn.is_empty package then None
    else
      let within_package =
        Names.Module_qn.of_parts
          (Names.Module_qn.parts package @ Names.Module_qn.parts target)
      in
      stored_at (Names.Module_qn.to_string within_package)

let build_scope_table
    ~(lang : Lang.t)
    ~(cfg : Index_lang_rules.t)
    ~(definitions_by_qn : definition Common.SMap.t)
    ~(file_funcs_index : (string, Func_info.t list) Hashtbl.t)
    ~(attributes_by_module : Func_lookup.module_attributes)
    ~(dunder_all : (string, unit) Hashtbl.t Common.SMap.t)
    ~(classes_by_file : class_info list Common.SMap.t)
    ~(class_parent_paths :
        (Function_id.t * IL.name option list) list Common.SMap.t)
    ~(resolution_orders : Func_lookup.resolution_orders)
    ~(methods_by_class : Func_lookup.methods_by_class)
    ~(extensions_by_module : Func_info.t list Common.SMap.t Common.SMap.t)
    ~(nested_types_by_class : Names.Class_qn.t Common.SMap.t Common.SMap.t)
    ~(global_imports : import list)
    ~(php_region_bindings : Scope_php.region_bindings Common.SMap.t)
    ~(php_global_bindings : Scope_binding.positioned_binding list)
    ~(module_scope : Scope_module.project_scope)
    (fi : file_info) : file_scope option =
  if
    not (resolves_by_binding lang)
    || cfg.Index_lang_rules.is_stub_file fi.fi_file
  then None
  else
    match cfg.Index_lang_rules.unqualified_scope with
    | `Per_package ->
      let bindings, bound_class_files =
        Scope_package.build ~lang ~definitions_by_qn ~attributes_by_module
          ~classes_by_file ~class_parent_paths ~file_funcs_index
          ~resolution_orders ~methods_by_class ~extensions_by_module
          ~nested_types_by_class ~global_imports fi
      in
      Some { scope_table = Func_lookup.scope_table_of_map bindings;
             bound_class_files; own_modules = []; module_aliases = None }
    | `Per_namespace ->
      let bindings, own_modules =
        Scope_php.build ~definitions_by_qn
          ~region_bindings:php_region_bindings
          ~global_bindings:php_global_bindings
          ~classes_by_file ~class_parent_paths ~file_funcs_index fi
      in
      Some { scope_table = Func_lookup.scope_table_of_map bindings;
             bound_class_files = []; own_modules;
             module_aliases = None }
    | `Per_module ->
      let bound =
        Scope_module.build ~scope:module_scope ~classes_by_file
          ~class_parent_paths ~file_funcs_index fi
      in
      Some { scope_table =
               Func_lookup.scope_table_of_map bound.Scope_module.fb_scope;
             bound_class_files = bound.Scope_module.fb_class_files;
             own_modules = bound.Scope_module.fb_own_modules;
             module_aliases = Some bound.Scope_module.fb_module_aliases }
    | `Per_file
    | `Per_directory ->
    let fi_file_str = Fpath.to_string fi.fi_file in
    let package =
      Module_paths.enclosing_package ~cfg ~file:fi.fi_file fi.fi_module_path
    in
    let own_bindings =
      Scope_binding.own_definitions_of_file ~file_funcs_index ~fi_file_str
    in
    let own_classes =
      Scope_binding.own_class_bindings ~class_parent_paths
        ~binds_at_file_scope:(fun (owner : Names.Class_qn.t) ->
          String.equal (Names.Class_qn.to_string owner)
            (Names.Module_qn.to_string fi.fi_module_path))
        ~scope_of_owner:(fun _ -> None)
        (Option.value (Common.SMap.find_opt fi_file_str classes_by_file)
           ~default:[])
    in
    let imported, bound_class_files =
      List.fold_left
        (fun ((imported : Scope_binding.positioned_binding list),
              (bound_class_files : (Names.Class_name.t * Fpath.t) list))
             (imp : import) ->
          let pos = Scope_binding.position_of_tok imp.im_tok in
          match Imports.binding_of imp with
          | Imports.Wildcard_from (target : Names.Module_qn.t) ->
            ( Common.SMap.fold
                (fun (name : string)
                     (attribute : Func_lookup.module_attribute)
                     (imported : Scope_binding.positioned_binding list) ->
                  if not (Reexports.star_exported ~dunder_all target name) then
                    imported
                  else
                    match attribute with
                    | Func_lookup.Attr_functions (funcs : Func_info.t list) ->
                      Scope_binding.function_binding_of ~pos ~parent_path:[]
                        name funcs
                      @ imported
                    | Func_lookup.Attr_class (class_qn : Names.Class_qn.t) ->
                      Scope_binding.class_binding_of ~pos ~parent_path:[] name
                        class_qn
                      :: imported
                    | Func_lookup.Attr_module _ -> imported)
                (Func_lookup.attributes_of_module attributes_by_module target)
                imported,
              bound_class_files )
          | Imports.Named_binding { local; target } -> (
            match definition_of_target ~definitions_by_qn ~package target with
            | Some (Function_definitions funcs) ->
              (Scope_binding.function_binding_of ~pos ~parent_path:[] local
                 funcs
               @ imported,
               bound_class_files)
            | Some (Class_definition { class_file; class_qn }) ->
              ( Scope_binding.class_binding_of ~pos ~parent_path:[] local
                  class_qn
                :: imported,
                (Names.Class_name.of_string
                   (Names.Module_qn.bare_name target), class_file)
                :: bound_class_files )
            | None -> (imported, bound_class_files)))
        ([], []) fi.fi_imports
    in
    Some { scope_table =
             Func_lookup.scope_table_of_map
               (Scope_binding.bindings_of_positioned
                  (own_bindings @ own_classes @ List.rev imported));
           bound_class_files; own_modules = []; module_aliases = None }

let narrow_methods_by_bound_files
    ~(bound_class_files : (Names.Class_name.t * Fpath.t) list)
    ~(file_of_func : Func_info.t -> string option)
    ~(caller_file : string)
    (ts : Type_state.t) : Type_state.t =
  match bound_class_files with
  | [] -> ts
  | _ :: _ ->
    let keep_file (cls : Names.Class_name.t) (file : string) : bool =
      String.equal file caller_file
      || List.exists
           (fun ((bound_cls : Names.Class_name.t), (bound_file : Fpath.t)) ->
             Names.Class_name.equal bound_cls cls
             && String.equal (Fpath.to_string bound_file) file)
           bound_class_files
    in
    Type_state.narrow ~keep_file ~file_of_func
      ~classes:(List.map fst bound_class_files) ts

let func_file_opt (func : Func_info.t) : string option =
  Option.map Fpath.to_string (Func_info.def_file_opt func)

let build_same_file_funcs_by_name
    ~(file_funcs_index : (string, Func_info.t list) Hashtbl.t)
    ~(fi_file_str : string)
  : (string, Func_info.t list) Hashtbl.t =
  let same_file_list =
    Option.value (Hashtbl.find_opt file_funcs_index fi_file_str)
      ~default:[]
  in
  let tbl = Hashtbl.create (List.length same_file_list) in
  List.iter (fun (func : Func_info.t) ->
    match List_.init_and_last_opt func.Func_info.fn_id with
    | Some (_, Some bare_name) ->
      let name = fst bare_name.IL.ident in
      let cur = Option.value (Hashtbl.find_opt tbl name) ~default:[] in
      Hashtbl.replace tbl name (func :: cur)
    | _ -> ()
  ) same_file_list;
  tbl

let build_funcs_by_name
    ~(visible : (string, unit) Hashtbl.t)
    ~(project_funcs_by_name : (string, Func_info.t list) Hashtbl.t)
    ~(func_in_caller_file : Func_info.t -> bool)
    () : (string, Func_info.t list) Hashtbl.t option =
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
      let same, other = List.partition func_in_caller_file kept in
      let kept = same @ other in
      if kept <> [] then Hashtbl.replace tbl name kept
  ) visible;
  Some tbl

(* Imported module singletons: stamp [local]'s occurrences with the
   singleton's class. *)
let stamp_singleton_imports
    ~(type_state : Type_state.t)
    (fi : file_info) : unit =
  let facts =
    List.fold_left (fun acc (imp : import) ->
      let local = imp.im_local in
      if String.length local = 0 then acc
      else
        match Type_state.get_module_singleton type_state imp.im_target with
        | None -> acc
        | Some ty ->
          let v_id =
            G.Id ((local, Tok.unsafe_fake_tok local), G.empty_id_info ())
          in
          (v_id, ty) :: acc
    ) [] fi.fi_imports
  in
  Object_initialization.stamp_id_types facts fi.fi_ast

let construction_resolver ~(lang : Lang.t) ~(type_state : Type_state.t)
    ~(func_lookup : Func_lookup.t)
    ~(caller_parent_path : IL.name option list)
    ~(call_arity : int) (ty : G.type_) : Func_info.fn_id option =
  Option.bind (Callee_resolution.expr_of_type_name ty)
    (fun (callee : G.expr) ->
      Callee_resolution.identify_callee_interfile ~lang ~type_state
        ~func_lookup ~caller_parent_path ~call_arity ~allow_constructor:true
        callee)

let invocation_resolver ~(func_lookup : Func_lookup.t)
    ~(caller_parent_path : IL.name option list) (var_name : string)
    : Func_info.fn_id option =
  match
    Func_lookup.functions_of_entries
      (Callee_resolution.nearest_scope_entries
         (Func_lookup.resolve_in_scope func_lookup var_name)
         caller_parent_path)
  with
  | (func : Func_info.t) :: _ -> Some func.Func_info.fn_id
  | [] -> None

let edges_for_file (ctx : ctx) (fi : file_info)
  : (Function_id.t * Function_id.t * Tok.t) list =
  let { lang; cfg; type_state; required_files_narrowing; definitions_by_qn;
        attributes_by_module; dunder_all;
        resolution_orders; class_qn_by_definition; methods_by_class;
        extensions_by_module; nested_types_by_class;
        php_region_bindings; php_global_bindings; module_scope;
        classes_by_file; class_parent_paths; global_imports;
        project_constructors;
        project_funcs_by_name; project_funcs_by_module; file_module_qn;
        project_funcs_by_package; project_class_names;
        file_funcs_index;
        slice_element_of_field;
        top_level_node_for; visible_names_for_file;
        stamp_var_types; value_alias_index } = ctx in
  let skip_anon (opt_ent : G.entity option) =
    not cfg.Index_lang_rules.include_anonymous_funcs && Option.is_none opt_ent
  in
    let emitter =
      Edge_emitter.create ~top_level:(top_level_node_for fi.fi_file)
    in
    let visible = staged "visibility" (fun () -> visible_names_for_file fi) in
    let fi_file_str = Fpath.to_string fi.fi_file in
    let top_level_node = top_level_node_for fi.fi_file in
    let func_in_caller_file (func : FA.func_info) : bool =
      match func_file_opt func with
      | Some file_str -> file_str = fi_file_str
      | None -> false
    in
    (* Must run before [build_funcs_by_name]: augments [visible] with cross-file class targets it filters on. *)
    staged "stamp base var types + import aliases" (fun () ->
      stamp_base_var_types ~lang ~project_class_names ~visible fi;
      stamp_import_value_aliases ~value_alias_index fi);
    let file_scope =
      staged "scope table" @@ fun () ->
      build_scope_table ~lang ~cfg ~definitions_by_qn
        ~file_funcs_index ~attributes_by_module ~dunder_all ~classes_by_file
        ~class_parent_paths ~resolution_orders ~methods_by_class
        ~extensions_by_module ~nested_types_by_class ~global_imports
        ~php_region_bindings ~php_global_bindings ~module_scope fi
    in
    let alias_to_module_qn =
      staged "alias to module map" @@ fun () ->
      match file_scope with
      | Some { module_aliases = Some (aliases : (string, Names.Module_qn.t) Hashtbl.t); _ } ->
        if Int.equal (Hashtbl.length aliases) 0 then None else Some aliases
      | Some { module_aliases = None; _ }
      | None -> build_alias_to_module_qn ~cfg fi
    in
    let funcs_by_module_qn
      : (Names.Module_qn.t, FA.func_info list) Hashtbl.t option =
      match cfg.Index_lang_rules.unqualified_scope with
      | `Per_file | `Per_directory -> Some project_funcs_by_module
      | `Per_package | `Per_namespace | `Per_module -> None
    in
    let file_funcs_by_package =
      staged "file funcs by package" @@ fun () ->
      build_file_funcs_by_package ~cfg ~project_funcs_by_package
        ~project_funcs_by_module fi
    in
    let file_type_state =
      staged "narrow methods by imports/required files" @@ fun () ->
      match file_scope with
      | Some (scope : file_scope) ->
        narrow_methods_by_bound_files
          ~bound_class_files:scope.bound_class_files
          ~file_of_func:func_file_opt ~caller_file:fi_file_str type_state
      | None ->
      let base =
        cfg.Index_lang_rules.narrow_methods_by_imports
          ~fi_imports:
            (List.map (fun (imp : import) -> (imp.im_local, imp.im_target))
               fi.fi_imports)
          ~file_of_func:func_file_opt type_state
      in
      match required_files_narrowing with
        | Some (narrowing : required_files_narrowing) ->
          let required_specs =
            List.filter_map (fun (local, spec, _kind) ->
              if String.equal local "*" then Some spec else None)
              fi.fi_import_specifiers
          in
          narrow_methods_by_required_files ~required_specs
            ~file_of_func:func_file_opt ~caller_file:fi_file_str ~narrowing
            base
        | None -> base
    in
    let same_file_funcs_by_name =
      staged "same-file funcs table" @@ fun () ->
      build_same_file_funcs_by_name ~file_funcs_index ~fi_file_str
    in
    let funcs_by_name =
      staged "funcs_by_name table" @@ fun () ->
      build_funcs_by_name ~visible ~project_funcs_by_name ~func_in_caller_file ()
    in
    let func_lookup =
      staged "func_lookup create" @@ fun () ->
      Func_lookup.create
        ?funcs_by_name:(Option.map Func_lookup.bare_name_index_of_hashtbl funcs_by_name)
        ~project_funcs_by_name:
          (Func_lookup.bare_name_index_of_hashtbl project_funcs_by_name)
        ?funcs_by_module_qn:
          (Option.map Func_lookup.module_index_of_hashtbl funcs_by_module_qn)
        ?alias_to_module_qn:
          (Option.map Func_lookup.alias_index_of_hashtbl alias_to_module_qn)
        ~same_file_funcs_by_name:
          (Func_lookup.bare_name_index_of_hashtbl same_file_funcs_by_name)
        ~overload_groups:(Lang_config.overloads_by_type lang)
        ?funcs_by_package:file_funcs_by_package
        ~file_module_qn:
          (Func_lookup.file_module_index_of_hashtbl file_module_qn)
        ?constructors:
          (Option.bind funcs_by_name
             (Func_lookup.constructor_index_of_hashtbl ~lang))
        ~project_constructors
        ~module_attributes:attributes_by_module
        ~resolution_orders
        ~class_qn_by_definition
        ~methods_by_class
        ~scope_table:
          (match file_scope with
           | Some (scope : file_scope) -> scope.scope_table
           | None -> Func_lookup.empty_scope_table)
        ~own_modules:
          (match file_scope with
           | Some (scope : file_scope) -> scope.own_modules
           | None -> [])
        ()
    in
    staged "stamp var types" (fun () ->
      stamp_singleton_imports ~type_state fi;
      (* the file's view: the classes it imports, a method's return type
         from the class it sees *)
      stamp_var_types ~type_state:file_type_state ~slice_element_of_field
        fi.fi_ast);
    let per_fdef_edges =
      staged "extract calls" @@ fun () ->
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
                  (match Ty_bare_name.inner_class_name_of_ty pty with
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
                List.map (fun name -> (mk name, cls_id)) (Receiver.self_names lang)
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
                         let ty_bare_name = match ty_n with
                           | G.Id _ -> ty_n
                           | G.IdQualified { name_last = ((str, tok), _); _ } ->
                             G.Id ((str, tok), G.empty_id_info ())
                         in
                         (var_n, ty_bare_name) :: acc
                       | _ -> acc)
                    | _ -> acc) [] body_stmt
            in
            isinstance_facts @ self_facts @ param_facts
          in
          Object_initialization.stamp_id_types fdef_facts body_program;
          stamp_var_types ~type_state:file_type_state ~slice_element_of_field
            body_program;
          let { FA.calls = callee_calls; callbacks = callback_calls; _ } =
            FA.extract_calls ~lang
              ~identify_callee:
                (Callee_resolution.identify_callee_interfile ~lang
                   ~type_state:file_type_state)
              ~identify_callback:
                (Callback_extraction.identify_callback_interfile ~lang
                   ~type_state:file_type_state)
              ~resolve_construction:
                (construction_resolver ~lang ~type_state:file_type_state
                   ~func_lookup ~caller_parent_path:fn_id)
              ~resolve_invocation:
                (invocation_resolver ~func_lookup)
              ~func_lookup
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
               FA.extract_decorator_calls
                 ~identify_callee:
                   (Callee_resolution.identify_callee_interfile ~lang
                   ~type_state:file_type_state)
                 ~func_lookup
                 ~caller_parent_path:fn_id ent.G.attrs
             in
             List.fold_left (fun edges (callee, call_tok) ->
               Edge_emitter.emit_call emitter ~caller_node:caller_node_opt
                 ~is_toplevel_lambda ~callee ~call_tok @ edges
             ) edges dec_calls))
      [] fi.fi_ast
    in
    let toplevel_calls =
      FA.extract_toplevel_calls
        ~identify_callee:
          (Callee_resolution.identify_callee_interfile ~lang
                   ~type_state:file_type_state)
        ~func_lookup
        fi.fi_ast
    in
    let toplevel_call_edges =
      List.fold_left (fun edges (callee, call_tok) ->
        Edge_emitter.emit_toplevel emitter ~callee ~call_tok @ edges
      ) [] toplevel_calls
    in
    let toplevel_callbacks =
      (* The file's visible functions ahead of the project's, layered
         rather than merged: a merge copied the project table once per
         file. *)
      let project_index =
        Func_lookup.bare_name_index_of_hashtbl project_funcs_by_name
      in
      let merged_funcs_by_name =
        match funcs_by_name with
        | None -> project_index
        | Some pf ->
          Func_lookup.bare_name_index_layered
            ~front:(Func_lookup.bare_name_index_of_hashtbl pf)
            ~back:project_index
      in
      let toplevel_func_lookup =
        Func_lookup.create
          ~funcs_by_name:merged_funcs_by_name
          ~overload_groups:(Lang_config.overloads_by_type lang)
          ~module_attributes:attributes_by_module
          ~resolution_orders
          ~class_qn_by_definition
          ~methods_by_class
          ?alias_to_module_qn:
            (Option.map Func_lookup.alias_index_of_hashtbl alias_to_module_qn)
          ~scope_table:
            (match file_scope with
             | Some (scope : file_scope) -> scope.scope_table
             | None -> Func_lookup.empty_scope_table)
          ~own_modules:
            (match file_scope with
             | Some (scope : file_scope) -> scope.own_modules
             | None -> [])
          ()
      in
      FA.extract_toplevel_hof_callbacks ~lang
        ~identify_callback:
          (Callback_extraction.identify_callback_interfile ~lang
             ~type_state:file_type_state)
        ~func_lookup:toplevel_func_lookup fi.fi_ast
    in
    let toplevel_callback_edges =
      List.fold_left (fun edges (callback, call_tok) ->
        Edge_emitter.emit_toplevel emitter ~callee:callback ~call_tok @ edges
      ) [] toplevel_callbacks
    in
    per_fdef_edges @ toplevel_call_edges @ toplevel_callback_edges
