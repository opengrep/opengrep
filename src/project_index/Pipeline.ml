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
  table:Symbol_table.t ->
  type_state:Type_state.t ->
  caller:Function_id.t option ->
  G.program ->
  unit

type file_scope = {
  scope_table : Func_lookup.scope_table;
  own_modules : Names.Module_qn.t list;
  module_aliases : (string, Names.Module_qn.t) Hashtbl.t option;
  member_classes : Names.Class_qn.t list;
}

type project_classes = {
  class_table : Class_table.t;
  class_of_qn : Names.Class_qn.t -> Class_table.cls option;
}

type ctx = {
  lang : Lang.t;
  cfg : Index_lang_rules.t;
  type_state : Type_state.t;
  definitions_by_qn : definition Common.SMap.t;
  companions : Func_lookup.companion_index;
  attributes_by_module : Func_lookup.module_attributes;
  dunder_all : (string, unit) Hashtbl.t Common.SMap.t;
  object_classes : unit Common.SMap.t;
  extensions_by_module : Func_info.t list Common.SMap.t Common.SMap.t;
  nested_types_by_class : Names.Class_qn.t Common.SMap.t Common.SMap.t;
  namespace_scope_bindings : Scope_binding.namespace_scope_bindings Common.SMap.t;
  php_global_bindings : Scope_binding.positioned_binding list;
  include_map : Include_map.t;
  module_scope : Scope_module.project_scope;
  go_packages : Scope_go.package_index;
  top_level_scope : Func_lookup.scope_table;
  module_object_by_module : Names.Class_qn.t Common.SMap.t;
  classes_by_file : entry list Common.SMap.t;
  class_parent_paths : (Function_id.t * IL.name option list) list Common.SMap.t;
  global_imports : import list;
  project_funcs_by_name : (string, Func_info.t list) Hashtbl.t;
  project_funcs_by_module :
    (Names.Module_qn.t, Func_info.t list) Hashtbl.t;
  file_module_qn : (string, Names.Module_qn.t) Hashtbl.t;
  project_class_names : Object_initialization.class_names;
  file_funcs_index : (string, Func_info.t list) Hashtbl.t;
  top_level_node_for : Fpath.t -> Function_id.t;
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
   [id_instance_type]. *)
let stamp_base_var_types
    ~(lang : Lang.t)
    ~(project_class_names : Object_initialization.class_names)
    (fi : file_info) : unit =
  let facts =
    Object_initialization.detect_object_initialization
      ~extra_class_names:project_class_names fi.fi_ast lang
  in
  Object_initialization.stamp_id_types facts fi.fi_ast

let build_alias_to_module_qn
    ~(cfg : Index_lang_rules.t) (fi : file_info)
  : (string, Names.Module_qn.t) Hashtbl.t option =
  match cfg.Index_lang_rules.unqualified_scope with
  | `Per_module
  | `Per_go_package -> None
  | `Per_file | `Per_crate | `Per_directory | `Per_namespace | `Per_project ->
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

let resolves_by_binding (lang : Lang.t) : bool =
  match lang with
  | Lang.Python | Lang.Python2 | Lang.Python3
  | Lang.Java | Lang.Kotlin | Lang.Csharp | Lang.Php
  | Lang.Js | Lang.Ts | Lang.Go | Lang.Ruby | Lang.Rust | Lang.C
  | Lang.Cpp | Lang.Elixir | Lang.Clojure | Lang.Scala | Lang.Apex
  | Lang.Swift | Lang.Vb | Lang.Lua | Lang.Dart | Lang.Julia | Lang.Crystal ->
    true
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
    ~(classes_by_file : entry list Common.SMap.t)
    ~(class_parent_paths :
        (Function_id.t * IL.name option list) list Common.SMap.t)
    ~(object_classes : unit Common.SMap.t)
    ~(extensions_by_module : Func_info.t list Common.SMap.t Common.SMap.t)
    ~(nested_types_by_class : Names.Class_qn.t Common.SMap.t Common.SMap.t)
    ~(global_imports : import list)
    ~(namespace_scope_bindings : Scope_binding.namespace_scope_bindings Common.SMap.t)
    ~(php_global_bindings : Scope_binding.positioned_binding list)
    ~(include_map : Include_map.t)
    ~(module_scope : Scope_module.project_scope)
    ~(go_packages : Scope_go.package_index)
    ~(top_level_scope : Func_lookup.scope_table)
    ~(module_object_by_module : Names.Class_qn.t Common.SMap.t)
    (fi : file_info) : file_scope option =
  if
    not (resolves_by_binding lang)
    || cfg.Index_lang_rules.is_stub_file fi.fi_file
  then None
  else
    match cfg.Index_lang_rules.unqualified_scope with
    | `Per_package ->
      let bindings, member_classes =
        Scope_package.build ~precedence:cfg.Index_lang_rules.precedence
          ~own_package_members_kind:cfg.Index_lang_rules.own_package_members_kind
          ~namespaces_nest:cfg.Index_lang_rules.namespaces_nest
          ~definitions_by_qn ~attributes_by_module
          ~classes_by_file ~class_parent_paths ~file_funcs_index
          ~extensions_by_module ~nested_types_by_class ~global_imports
          ~object_classes
          ~companions:(Lang_config.companion_object_has_own_name lang) fi
      in
      Some { scope_table = Func_lookup.scope_table_of_map bindings;
             own_modules = []; module_aliases = None;
             member_classes }
    | `Per_go_package ->
      let bindings, module_aliases =
        Scope_go.build ~lang ~cfg ~package_index:go_packages
          ~attributes_by_module ~classes_by_file ~class_parent_paths
          ~file_funcs_index fi
      in
      Some { scope_table = Func_lookup.scope_table_of_map bindings;
             own_modules = [];
             module_aliases = Some module_aliases;
             member_classes = [] }
    | `Per_project ->
      Some { scope_table =
               Scope_project.build ~classes_by_file ~class_parent_paths
                 ~file_funcs_index ~module_object_by_module ~top_level_scope fi;
             own_modules = [];
             module_aliases = None;
             member_classes = [] }
    | `Per_constant_path ->
      let bindings, own_modules =
        Scope_ruby.build ~classes_by_file ~class_parent_paths
          ~file_funcs_index ~nested_types_by_class fi
      in
      Some { scope_table =
               Func_lookup.scope_table_union
                 ~front:(Func_lookup.scope_table_of_map bindings)
                 ~back:top_level_scope;
             own_modules;
             module_aliases = None;
             member_classes = [] }
    | `Per_namespace ->
      let bindings, own_modules =
        Scope_php.build ~definitions_by_qn ~attributes_by_module
          ~namespace_scope_bindings
          ~global_bindings:php_global_bindings
          ~classes_by_file ~class_parent_paths ~file_funcs_index fi
      in
      Some { scope_table = Func_lookup.scope_table_of_map bindings;
             own_modules;
             module_aliases = None;
             member_classes = [] }
    | `Per_translation_unit ->
      let closure =
        Include_map.closure_of_file include_map (Fpath.to_string fi.fi_file)
      in
      let bindings =
        Scope_c_family.build ~precedence:cfg.Index_lang_rules.precedence
          ~definitions_by_qn ~attributes_by_module
          ~classes_by_file ~class_parent_paths ~namespace_scope_bindings
          ~include_bindings:
            (Include_map.bindings_of_file include_map ~closure
               (Fpath.to_string fi.fi_file))
          ~included_files:(Include_map.files_in_closure closure)
          ~file_funcs_index fi
      in
      Some { scope_table = Func_lookup.scope_table_of_map bindings;
             own_modules = fi.fi_namespace_scopes;
             module_aliases = None;
             member_classes = [] }
    | `Per_crate ->
      let bindings, module_aliases =
        Scope_rust.build ~cfg ~definitions_by_qn ~attributes_by_module
          ~classes_by_file ~class_parent_paths ~file_funcs_index
          ~import_aliases:(build_alias_to_module_qn ~cfg fi) fi
      in
      Some { scope_table = Func_lookup.scope_table_of_map bindings;
             own_modules = [];
             module_aliases = Some module_aliases;
             member_classes = [] }
    | `Per_module ->
      let bound =
        Scope_module.build ~scope:module_scope ~classes_by_file
          ~class_parent_paths ~file_funcs_index fi
      in
      Some { scope_table =
               Func_lookup.scope_table_of_map bound.Scope_module.fb_scope;
             own_modules = bound.Scope_module.fb_own_modules;
             module_aliases = Some bound.Scope_module.fb_module_aliases;
             member_classes = [] }
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
      Scope_binding.own_class_bindings ~companion:Scope_binding.no_companion
        ~class_parent_paths
        ~binds_at_file_scope:(fun (owner : Names.Class_qn.t) ->
          String.equal (Names.Class_qn.to_string owner)
            (Names.Module_qn.to_string fi.fi_module_path))
        ~scope_of_owner:(fun _ -> None)
        (Option.value (Common.SMap.find_opt fi_file_str classes_by_file)
           ~default:[])
    in
    let imported =
      List.fold_left
        (fun (imported : Scope_binding.positioned_binding list)
             (imp : import) ->
          let pos = Scope_binding.position_of_tok imp.im_tok in
          match Imports.binding_of imp with
          | Imports.Wildcard_from (target : Names.Module_qn.t) ->
            Common.SMap.fold
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
                    | Func_lookup.Attr_class_with_companion
                        (class_qn, companion_qn) ->
                      Scope_binding.class_binding_of ~pos ~parent_path:[] name
                        class_qn
                      :: Scope_binding.companion_binding_of ~pos ~parent_path:[]
                           name companion_qn
                      :: imported
                    | Func_lookup.Attr_module _ -> imported)
                (Func_lookup.attributes_of_module attributes_by_module target)
                imported
          | Imports.Named_binding { local; target } -> (
            match definition_of_target ~definitions_by_qn ~package target with
            | Some (Function_definitions funcs) ->
              Scope_binding.function_binding_of ~pos ~parent_path:[] local
                funcs
              @ imported
            | Some (Class_definition { class_qn; class_companion; _ }) ->
              Scope_binding.class_binding_of ~pos ~parent_path:[] local
                class_qn
              :: (match class_companion with
                  | Some (companion_qn : Names.Class_qn.t) ->
                    [ Scope_binding.companion_binding_of ~pos ~parent_path:[]
                        local companion_qn ]
                  | None -> [])
              @ imported
            | None -> imported))
        [] fi.fi_imports
    in
    Some { scope_table =
             Func_lookup.scope_table_of_map
               (Scope_binding.bindings_of_positioned
                  (own_bindings @ own_classes @ List.rev imported));
           own_modules = []; module_aliases = None;
             member_classes = [] }

let file_scope_of (ctx : ctx) (fi : file_info) : file_scope option =
  build_scope_table ~lang:ctx.lang ~cfg:ctx.cfg
    ~definitions_by_qn:ctx.definitions_by_qn
    ~file_funcs_index:ctx.file_funcs_index
    ~attributes_by_module:ctx.attributes_by_module ~dunder_all:ctx.dunder_all
    ~classes_by_file:ctx.classes_by_file
    ~class_parent_paths:ctx.class_parent_paths
    ~object_classes:ctx.object_classes
    ~extensions_by_module:ctx.extensions_by_module
    ~nested_types_by_class:ctx.nested_types_by_class
    ~global_imports:ctx.global_imports
    ~namespace_scope_bindings:ctx.namespace_scope_bindings
    ~php_global_bindings:ctx.php_global_bindings ~include_map:ctx.include_map
    ~module_scope:ctx.module_scope ~go_packages:ctx.go_packages
    ~top_level_scope:ctx.top_level_scope
    ~module_object_by_module:ctx.module_object_by_module fi

let alias_to_module_qn_of ~(cfg : Index_lang_rules.t)
    (file_scope : file_scope option) (fi : file_info)
    : (string, Names.Module_qn.t) Hashtbl.t option =
  match file_scope with
  | Some { module_aliases = Some (aliases : (string, Names.Module_qn.t) Hashtbl.t); _ } ->
    if Int.equal (Hashtbl.length aliases) 0 then None else Some aliases
  | Some { module_aliases = None; _ }
  | None -> build_alias_to_module_qn ~cfg fi

let attribute_of_definition (definition : definition)
    : Func_lookup.module_attribute =
  match definition with
  | Function_definitions (funcs : Func_info.t list) ->
    Func_lookup.Attr_functions funcs
  | Class_definition
      { class_qn; class_companion = Some (companion : Names.Class_qn.t); _ } ->
    Func_lookup.Attr_class_with_companion (class_qn, companion)
  | Class_definition { class_qn; class_companion = None; _ } ->
    Func_lookup.Attr_class class_qn

let func_lookup_of (ctx : ctx)
    ~(class_of_qn : Names.Class_qn.t -> Class_table.cls option)
    (file_scope : file_scope option) (fi : file_info) : Func_lookup.t =
  Func_lookup.create
    ?alias_to_module_qn:
      (Option.map Func_lookup.alias_index_of_hashtbl
         (alias_to_module_qn_of ~cfg:ctx.cfg file_scope fi))
    ~module_attributes:ctx.attributes_by_module
    ~companions:ctx.companions
    ~class_of_qn
    ~is_import:(fun (sid : G.SId.t) ->
      Option.is_some (Imports.import_of_binding fi.fi_imports sid))
    ~definition:(fun (qualified_name : string) ->
      Option.map attribute_of_definition
        (Common.SMap.find_opt qualified_name ctx.definitions_by_qn))
    ~scope_table:
      (match file_scope with
       | Some (scope : file_scope) -> scope.scope_table
       | None -> Func_lookup.empty_scope_table)
    ~own_modules:
      (match file_scope with
       | Some (scope : file_scope) -> scope.own_modules
       | None -> [])
    ~member_classes:
      (match file_scope with
       | Some (scope : file_scope) -> scope.member_classes
       | None -> [])
    ()

(* Imported module singletons: stamp [local]'s occurrences with the
   singleton's class. *)
let stamp_singleton_imports
    ~(type_state : Type_state.t) ~(class_table : Class_table.t)
    (fi : file_info) : unit =
  let facts =
    List.fold_left (fun acc (imp : import) ->
      let local = imp.im_local in
      if String.length local = 0 then acc
      else
        match
          Option.bind
            (Type_state.get_module_singleton type_state imp.im_target)
            (Class_table.name_of_class class_table)
        with
        | None -> acc
        | Some ty ->
          let v_id =
            G.Id ((local, Tok.unsafe_fake_tok local), G.empty_id_info ())
          in
          (v_id, ty) :: acc
    ) [] fi.fi_imports
  in
  Object_initialization.stamp_id_types facts fi.fi_ast

let defined_funcs (resolution : Symbol_table.resolution) : Func_info.t list =
  match resolution with
  | Symbol_table.Defined (funcs : Func_info.t list) -> funcs
  | Symbol_table.External -> []

let fn_ids_of (funcs : Func_info.t list) : Func_info.fn_id list =
  List_.map (fun (func : Func_info.t) -> func.Func_info.fn_id) funcs

let or_outside_file (resolved : Symbol_table.resolution)
    (outside : unit -> Symbol_table.resolution) : Symbol_table.resolution =
  match resolved with
  | Symbol_table.Defined _ -> resolved
  | Symbol_table.External -> outside ()

let resolve_in_project ~(lang : Lang.t) ~(table : Symbol_table.t)
    ~(func_lookup : Func_lookup.t)
    ~(caller_parent_path : IL.name option list) ~(use : Symbol_table.use)
    (e : G.expr) : Symbol_table.resolution =
  let caller = FA.fn_id_to_node caller_parent_path in
  let resolved =
    match use with
    | Symbol_table.Called -> Symbol_table.resolve_callee table ~caller e
    | Symbol_table.Referenced -> Symbol_table.resolve_reference table ~caller e
  in
  or_outside_file resolved (fun () ->
    Callee_resolution.resolve_outside_file ~lang ~table ~func_lookup ~caller
      ~caller_parent_path ~use e)

let call_site_resolver ~(lang : Lang.t) ~(table : Symbol_table.t)
    ~(func_lookup : Func_lookup.t) : Callee_resolution.call_site_resolver =
 fun ~caller_parent_path ~call_args (callee : G.expr) ->
  resolve_in_project ~lang ~table ~func_lookup ~caller_parent_path
    ~use:Symbol_table.Called callee
  |> defined_funcs
  |> Callee_resolution.narrow_by_call ~lang call_args
  |> fn_ids_of

let callback_resolver ~(lang : Lang.t) ~(table : Symbol_table.t)
    ~(func_lookup : Func_lookup.t)
    ~(caller_parent_path : IL.name option list)
    : Callback_extraction.callback_resolver =
 fun ~caller:_ (reference : Callback_extraction.reference) ->
  match reference with
  | Callback_extraction.Written ({ G.e = G.N (name : G.name); _ } as e) ->
    or_outside_file (Symbol_table.resolve_qualified table name) (fun () ->
      Callee_resolution.resolve_outside_file ~lang ~table ~func_lookup
        ~caller:(FA.fn_id_to_node caller_parent_path) ~caller_parent_path
        ~use:Symbol_table.Referenced e)
  | Callback_extraction.Bound (e : G.expr)
  | Callback_extraction.Written (e : G.expr) ->
    resolve_in_project ~lang ~table ~func_lookup ~caller_parent_path
      ~use:Symbol_table.Referenced e

let construction_resolver ~(lang : Lang.t) ~(table : Symbol_table.t)
    ~(func_lookup : Func_lookup.t)
    ~(caller_parent_path : IL.name option list)
    : Callee_resolution.construction_resolver =
 fun ~call_args (ty : G.type_) ->
  or_outside_file (Symbol_table.resolve_construction table ty) (fun () ->
    Callee_resolution.resolve_construction_outside_file ~table ~func_lookup
      ~caller_parent_path ty)
  |> defined_funcs
  |> Callee_resolution.narrow_by_call ~lang (Some call_args)
  |> fn_ids_of

let invocation_resolver ~(lang : Lang.t) ~(table : Symbol_table.t)
    ~(func_lookup : Func_lookup.t) : Callee_resolution.invocation_resolver =
 fun ~caller_parent_path (receiver : G.expr) ->
  resolve_in_project ~lang ~table ~func_lookup ~caller_parent_path
    ~use:Symbol_table.Referenced receiver
  |> defined_funcs
  |> fn_ids_of

let project_table (ctx : ctx) ~(classes : project_classes)
    ~(file_table : Symbol_table.t) (file_scope : file_scope option)
    (fi : file_info) : Symbol_table.t * Func_lookup.t =
  let func_lookup =
    func_lookup_of ctx ~class_of_qn:classes.class_of_qn file_scope fi
  in
  let outside (table : Symbol_table.t) ~(caller : Function_id.t option)
      (e : G.expr) : Symbol_table.resolution =
    let caller_parent_path =
      match Option.bind caller (Symbol_table.function_of_node table) with
      | Some (func : Func_info.t) -> func.Func_info.fn_id
      | None -> []
    in
    Callee_resolution.resolve_outside_file ~lang:ctx.lang ~table ~func_lookup
      ~caller ~caller_parent_path ~use:Symbol_table.Called e
  in
  ( Symbol_table.with_project file_table classes.class_table
      ~extension_visible:(Callee_resolution.extension_visible ~func_lookup)
      ~outside,
    func_lookup )

let edges_for_file (ctx : ctx) ~(classes : project_classes)
    ~(table : Symbol_table.t) ~(func_lookup : Func_lookup.t)
    (fi : file_info) : (Function_id.t * Function_id.t * Tok.t) list =
  let { lang; cfg; type_state;
        project_class_names;
        top_level_node_for;
        stamp_var_types; value_alias_index; _ } = ctx in
  let skip_anon (opt_ent : G.entity option) =
    not cfg.Index_lang_rules.include_anonymous_funcs && Option.is_none opt_ent
  in
    let emitter =
      Edge_emitter.create ~top_level:(top_level_node_for fi.fi_file)
    in
    let top_level_node = top_level_node_for fi.fi_file in
    staged "stamp base var types + import aliases" (fun () ->
      stamp_base_var_types ~lang ~project_class_names fi;
      stamp_import_value_aliases ~value_alias_index fi);
    let table = Symbol_table.with_types table type_state in
    staged "stamp var types" (fun () ->
      stamp_singleton_imports ~type_state ~class_table:classes.class_table fi;
      (* the file's view: the classes it imports, a method's return type
         from the class it sees *)
      stamp_var_types ~table ~type_state ~caller:None fi.fi_ast);
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
            let stamped (cls : Class_table.cls) : G.name option =
              Class_table.name_of_class classes.class_table cls
            in
            let param_facts =
              Tok.unbracket fdef.G.fparams
              |> List.filter_map (fun param ->
                match param with
                | G.ParamReceiver { G.pname = Some pn; ptype = Some pty; _ }
                | G.Param { G.pname = Some pn; ptype = Some pty; _ } ->
                  (match
                     Option.bind
                       (Symbol_table.class_of_declared_type table ~context:None
                          (Ty_bare_name.inner_named_type pty))
                       stamped
                   with
                   | Some cls -> Some (G.Id (pn, G.empty_id_info ()), cls)
                   | None -> None)
                | _ -> None)
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
                      (match
                         ( var_e.G.e,
                           Symbol_table.receiver_class table
                             ~caller:caller_node_opt ty_e )
                       with
                       | G.N (G.Id _ as var_n), Symbol_table.Class_object cls ->
                         (match stamped cls with
                          | Some (ty_name : G.name) -> (var_n, ty_name) :: acc
                          | None -> acc)
                       | _ -> acc)
                    | _ -> acc) [] body_stmt
            in
            isinstance_facts @ param_facts
          in
          Object_initialization.stamp_id_types fdef_facts body_program;
          stamp_var_types ~table ~type_state ~caller:caller_node_opt
            body_program;
          let { FA.calls = callee_calls; callbacks = callback_calls; _ } =
            FA.extract_calls ~lang
              ~identify_callee:(call_site_resolver ~lang ~table ~func_lookup)
              ~resolve_callback:
                (callback_resolver ~lang ~table ~func_lookup
                   ~caller_parent_path:fn_id)
              ~resolve_construction:
                (construction_resolver ~lang ~table ~func_lookup
                   ~caller_parent_path:fn_id)
              ~resolve_invocation:
                (invocation_resolver ~lang ~table ~func_lookup)
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
                 ~identify_callee:(call_site_resolver ~lang ~table ~func_lookup)
                 ~caller_parent_path:fn_id ent.G.attrs
             in
             List.fold_left (fun edges (callee, call_tok) ->
               Edge_emitter.emit_call emitter ~caller_node:caller_node_opt
                 ~is_toplevel_lambda ~callee ~call_tok @ edges
             ) edges dec_calls))
      [] fi.fi_ast
    in
    let toplevel_calls =
      FA.extract_toplevel_calls ~lang
        ~identify_callee:(call_site_resolver ~lang ~table ~func_lookup)
        fi.fi_ast
    in
    let toplevel_call_edges =
      List.fold_left (fun edges (callee, call_tok) ->
        Edge_emitter.emit_toplevel emitter ~callee ~call_tok @ edges
      ) [] toplevel_calls
    in
    let toplevel_callbacks =
      FA.extract_toplevel_hof_callbacks ~lang
        ~resolve_callback:
          (callback_resolver ~lang ~table ~func_lookup ~caller_parent_path:[])
        fi.fi_ast
    in
    let toplevel_callback_edges =
      List.fold_left (fun edges (callback, call_tok) ->
        Edge_emitter.emit_toplevel emitter ~callee:callback ~call_tok @ edges
      ) [] toplevel_callbacks
    in
    per_fdef_edges @ toplevel_call_edges @ toplevel_callback_edges
