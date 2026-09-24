module G = AST_generic
open Types

type export_target =
  | Exports_definition of definition
  | Exports_module of Names.Module_qn.t
  | Exports_object of Func_info.t list Common.SMap.t

type exports = export_target Common.SMap.t Common.SMap.t

type export_row =
  | Export_name of { exported : string; local : string }
  | Export_from of { exported : string; target : Names.Module_qn.t }
  | Export_module of { exported : string; target : Names.Module_qn.t }
  | Export_star of Names.Module_qn.t

type file_rows = {
  fr_file : file_info;
  fr_rows : (string * export_row) list;
  fr_objects : (string * string * Func_info.t list Common.SMap.t) list;
}

let cjs_exports_object (e : G.expr) : bool =
  match e.G.e with
  | G.DotAccess ({ G.e = G.N (G.Id (("module", _), _)); _ }, _,
                 G.FN (G.Id (("exports", _), _))) -> true
  | _ -> false

let cjs_exported_name (e : G.expr) : string option =
  match e.G.e with
  | G.DotAccess (receiver, _, G.FN (G.Id ((name, _), _)))
    when cjs_exports_object receiver -> Some name
  | G.DotAccess ({ G.e = G.N (G.Id (("exports", _), _)); _ }, _,
                 G.FN (G.Id ((name, _), _))) -> Some name
  | _ -> None

let bare_name_of (e : G.expr) : string option =
  match e.G.e with
  | G.N (G.Id ((name, _), _)) -> Some name
  | _ -> None

let cjs_rows (lhs : G.expr) (rhs : G.expr) : export_row list =
  if cjs_exports_object lhs then
    match bare_name_of rhs with
    | Some (local : string) -> [ Export_name { exported = "default"; local } ]
    | None ->
      List.map
        (fun (((key : string), _), (local : string)) ->
          Export_name { exported = key; local })
        (Imports.record_field_names rhs)
  else
    match (cjs_exported_name lhs, bare_name_of rhs) with
    | Some (exported : string), Some (local : string) ->
      [ Export_name { exported; local } ]
    | Some _, None
    | None, _ -> []

let rows_of_stmt (stmt : G.stmt) : export_row list =
  match stmt.G.s with
  | G.DirectiveStmt { G.d = G.OtherDirective (("Export", _), [ G.I (name, _) ]);
                      _ } ->
    [ Export_name { exported = name; local = name } ]
  | G.ExprStmt ({ G.e = G.Assign (lhs, _, rhs); _ }, _) -> cjs_rows lhs rhs
  | _ -> []

let namespace_scope_key (module_path : Names.Module_qn.t) (namespace_scopes : string list list)
    : string =
  Names.Module_qn.to_string
    (Names.Module_qn.of_parts
       ((if Names.Module_qn.is_empty module_path then []
         else Names.Module_qn.parts module_path)
        @ List.concat (List.rev namespace_scopes)))

let object_members ~(own_funcs : Func_info.t list) (init : G.expr)
    : Func_info.t list Common.SMap.t option =
  let defined_at (identity : IL.name) : Func_info.t list =
    List.filter
      (fun (func : Func_info.t) ->
        match Func_info.bare_name func.Func_info.fn_id with
        | Some (bound : IL.name) -> Function_id.equal_il_name bound identity
        | None -> false)
      own_funcs
  in
  match init.G.e with
  | G.Record (_, fields, _) ->
    let members =
      List.fold_left
        (fun (members : Func_info.t list Common.SMap.t) (field : G.field) ->
          match field with
          | G.F { G.s = G.DefStmt ((ent : G.entity),
                                   G.FuncDef (fdef : G.function_definition));
                  _ } -> (
            match
              (ent.G.name, Visit_function_defs.func_il_for_entity ent fdef)
            with
            | G.EN (gname : G.name), Some (identity : IL.name) -> (
              match
                (Ty_bare_name.bare_name_of_name gname, defined_at identity)
              with
              | Some (name : string), ((_ :: _) as funcs) ->
                Common.SMap.add name funcs members
              | Some _, []
              | None, _ -> members)
            | G.EN _, None
            | G.EDynamic _, _
            | G.EPattern _, _
            | G.OtherEntity _, _ -> members)
          | _ -> members)
        Common.SMap.empty fields
    in
    if Common.SMap.is_empty members then None else Some members
  | _ -> None

let objects_of_stmt ~(own_funcs : Func_info.t list) (stmt : G.stmt)
    : (string * Func_info.t list Common.SMap.t) list =
  match stmt.G.s with
  | G.DefStmt ((ent : G.entity), G.VarDef { G.vinit = Some init; _ }) -> (
    match
      (Index_lang_rules.entity_simple_name ent, object_members ~own_funcs init)
    with
    | Some (name : string), Some (members : Func_info.t list Common.SMap.t) ->
      [ (name, members) ]
    | Some _, None
    | None, _ -> [])
  | _ -> []

let rows_of_file ~(file_funcs_index : (string, Func_info.t list) Hashtbl.t)
    (fi : file_info) : file_rows =
  let module_key = Names.Module_qn.to_string fi.fi_module_path in
  let own_funcs =
    Option.value
      (Hashtbl.find_opt file_funcs_index (Fpath.to_string fi.fi_file))
      ~default:[]
  in
  let rec walk (namespace_scopes : string list list)
      ((rows : (string * export_row) list),
       (objects : (string * string * Func_info.t list Common.SMap.t) list))
      (stmts : G.stmt list) :
      (string * export_row) list
      * (string * string * Func_info.t list Common.SMap.t) list =
    match stmts with
    | [] -> (rows, objects)
    | (stmt : G.stmt) :: rest -> (
      match stmt.G.s with
      | G.DirectiveStmt { G.d = G.Package (_, parts); _ } ->
        walk (List.map fst parts :: namespace_scopes) (rows, objects) rest
      | G.DirectiveStmt { G.d = G.PackageEnd _; _ } ->
        walk
          (match namespace_scopes with _ :: outer -> outer | [] -> [])
          (rows, objects) rest
      | _ ->
        let key = namespace_scope_key fi.fi_module_path namespace_scopes in
        walk namespace_scopes
          ( List.map (fun (row : export_row) -> (key, row)) (rows_of_stmt stmt)
            @ rows,
            List.map
              (fun ((name : string),
                    (members : Func_info.t list Common.SMap.t)) ->
                (key, name, members))
              (objects_of_stmt ~own_funcs stmt)
            @ objects )
          rest)
  in
  let from_imports =
    List.filter_map
      (fun (imp : import) ->
        match imp.im_role with
        | Role_binds -> None
        | Role_reexports -> (
          match (Imports.binding_of imp, imp.im_binds) with
          | Imports.Wildcard_from (target : Names.Module_qn.t), _ ->
            Some (module_key, Export_star target)
          | Imports.Named_binding { local; target }, Binds_module ->
            Some (module_key, Export_module { exported = local; target })
          | Imports.Named_binding { local; target }, Binds_any ->
            Some (module_key, Export_from { exported = local; target })
          | Imports.Named_binding _, (Binds_function | Binds_constant
                                     | Binds_type) -> None))
      fi.fi_imports
  in
  let rows, objects = walk [] ([], []) fi.fi_ast in
  { fr_file = fi; fr_rows = from_imports @ rows; fr_objects = objects }

let bound_in (exports : exports) (module_qn : Names.Module_qn.t)
    (name : string) : export_target option =
  Option.bind
    (Common.SMap.find_opt (Names.Module_qn.to_string module_qn) exports)
    (Common.SMap.find_opt name)

let export_of_target (exports : exports) (target : Names.Module_qn.t)
    : export_target option =
  Option.bind (Names.Module_qn.split_last target)
    (fun ((module_qn : Names.Module_qn.t), (name : string)) ->
      bound_in exports module_qn name)

let import_target_of (fi : file_info) (name : string) : import option =
  List.find_opt
    (fun (imp : import) ->
      String.equal imp.im_local name
      && (match imp.im_role with
          | Role_binds -> true
          | Role_reexports -> false))
    fi.fi_imports

let module_level_name ~(definitions_by_qn : definition Common.SMap.t)
    ~(value_alias_index : (string * string, G.expr) Hashtbl.t)
    ~(objects : Func_info.t list Common.SMap.t Common.SMap.t Common.SMap.t)
    ~(exports : exports) ~(module_qn : Names.Module_qn.t) (fi : file_info)
    (name : string) : export_target option =
  let module_key = Names.Module_qn.to_string fi.fi_module_path in
  let rec resolve (seen : string list) (name : string) : export_target option =
    if List.exists (String.equal name) seen then None
    else
      let within = Names.Module_qn.concat module_qn name in
      let own =
        Common.SMap.find_opt (Names.Module_qn.to_string within)
          definitions_by_qn
      in
      match own with
      | Some (found : definition) -> Some (Exports_definition found)
      | None when List.exists (Names.Module_qn.equal within)
                    fi.fi_namespace_scopes -> Some (Exports_module within)
      | None -> (
        match
          Option.bind
            (Common.SMap.find_opt (Names.Module_qn.to_string module_qn) objects)
            (Common.SMap.find_opt name)
        with
        | Some (members : Func_info.t list Common.SMap.t) ->
          Some (Exports_object members)
        | None -> (
        match import_target_of fi name with
        | Some (imp : import) -> (
          match imp.im_binds with
          | Binds_module -> Some (Exports_module imp.im_target)
          | Binds_type -> None
          | Binds_any
          | Binds_function
          | Binds_constant -> export_of_target exports imp.im_target)
        | None -> (
          match
            Option.bind (Hashtbl.find_opt value_alias_index (module_key, name))
              bare_name_of
          with
          | Some (alias : string) -> resolve (name :: seen) alias
          | None -> None)))
  in
  resolve [] name

let add_export (module_key : string) (name : string)
    (target : export_target) (exports : exports) : exports =
  Common.SMap.update module_key
    (function
      | None -> Some (Common.SMap.singleton name target)
      | Some (bound : export_target Common.SMap.t) ->
        Some (Common.SMap.add name target bound))
    exports

let one_pass ~(definitions_by_qn : definition Common.SMap.t)
    ~(value_alias_index : (string * string, G.expr) Hashtbl.t)
    ~(objects : Func_info.t list Common.SMap.t Common.SMap.t Common.SMap.t)
    (rows : file_rows list) (exports : exports) : exports * int =
  List.fold_left
    (fun ((exports : exports), (added : int)) (file : file_rows) ->
      List.fold_left
        (fun ((exports : exports), (added : int))
             ((module_key : string), (row : export_row)) ->
          let bind (name : string) (target : export_target option) =
            match target with
            | None -> (exports, added)
            | Some (target : export_target) ->
              if
                Option.fold ~none:false
                  ~some:(fun (bound : export_target Common.SMap.t) ->
                    Common.SMap.mem name bound)
                  (Common.SMap.find_opt module_key exports)
              then (exports, added)
              else (add_export module_key name target exports, added + 1)
          in
          match row with
          | Export_name { exported; local } ->
            bind exported
              (module_level_name ~definitions_by_qn ~value_alias_index ~objects
                 ~exports ~module_qn:(Names.Module_qn.of_string module_key)
                 file.fr_file local)
          | Export_from { exported; target } ->
            bind exported (export_of_target exports target)
          | Export_module { exported; target } ->
            bind exported (Some (Exports_module target))
          | Export_star (target : Names.Module_qn.t) ->
            Common.SMap.fold
              (fun (name : string) (bound : export_target)
                   ((exports : exports), (added : int)) ->
                if String.equal name "default" then (exports, added)
                else if
                  Option.fold ~none:false
                    ~some:(fun (already : export_target Common.SMap.t) ->
                      Common.SMap.mem name already)
                    (Common.SMap.find_opt module_key exports)
                then (exports, added)
                else (add_export module_key name bound exports, added + 1))
              (Option.value
                 (Common.SMap.find_opt (Names.Module_qn.to_string target)
                    exports)
                 ~default:Common.SMap.empty)
              (exports, added))
        (exports, added) file.fr_rows)
    (exports, 0) rows

let build_exports ~(definitions_by_qn : definition Common.SMap.t)
    ~(value_alias_index : (string * string, G.expr) Hashtbl.t)
    ~(objects : Func_info.t list Common.SMap.t Common.SMap.t Common.SMap.t)
    (rows : file_rows list) : exports =
  let rec fixpoint (exports : exports) : exports =
    match
      one_pass ~definitions_by_qn ~value_alias_index ~objects rows exports
    with
    | exports, 0 -> exports
    | exports, _ -> fixpoint exports
  in
  fixpoint Common.SMap.empty

let is_script (rows : file_rows) : bool =
  match (rows.fr_file.fi_imports, rows.fr_rows) with
  | [], [] -> true
  | _ :: _, _
  | _, _ :: _ -> false

let objects_by_module (rows : file_rows list)
    : Func_info.t list Common.SMap.t Common.SMap.t Common.SMap.t =
  List.fold_left
    (fun (by_module :
            Func_info.t list Common.SMap.t Common.SMap.t Common.SMap.t)
         (file : file_rows) ->
      List.fold_left
        (fun (by_module :
                Func_info.t list Common.SMap.t Common.SMap.t Common.SMap.t)
             (((module_key : string), (name : string),
               (members : Func_info.t list Common.SMap.t))) ->
          Common.SMap.update module_key
            (function
              | None -> Some (Common.SMap.singleton name members)
              | Some (bound : Func_info.t list Common.SMap.t Common.SMap.t) ->
                Some (Common.SMap.add name members bound))
            by_module)
        by_module file.fr_objects)
    Common.SMap.empty rows

let entries_of_positioned (bindings : Scope_binding.positioned_binding list)
    (entries : Func_lookup.scope_entry list Common.SMap.t)
    : Func_lookup.scope_entry list Common.SMap.t =
  List.fold_left
    (fun (entries : Func_lookup.scope_entry list Common.SMap.t)
         (binding : Scope_binding.positioned_binding) ->
      let bound =
        List.map
          (fun (kind : Func_lookup.scope_kind) ->
            { Func_lookup.kind;
              parent_path = binding.Scope_binding.pb_parent_path })
          binding.Scope_binding.pb_kinds
      in
      Common.SMap.update binding.Scope_binding.pb_name
        (function
          | None -> Some bound
          | Some (already : Func_lookup.scope_entry list) ->
            Some (bound @ already))
        entries)
    entries bindings

let exported_names (exports : exports) : unit Common.SMap.t Common.SMap.t =
  Common.SMap.map (Common.SMap.map (fun _ -> ())) exports

type project_scope = {
  pj_exports : exports;
  pj_scripts : Func_lookup.scope_entry list Common.SMap.t;
  pj_script_files : unit Common.SMap.t;
  pj_class_aliases : (Names.Class_qn.t * string * Func_info.t list) list;
  pj_objects : Func_info.t list Common.SMap.t Common.SMap.t Common.SMap.t;
}

let no_project_scope : project_scope =
  { pj_exports = Common.SMap.empty;
    pj_scripts = Common.SMap.empty;
    pj_script_files = Common.SMap.empty;
    pj_class_aliases = [];
    pj_objects = Common.SMap.empty }

let class_aliases_of (scope : project_scope)
    : (Names.Class_qn.t * string * Func_info.t list) list =
  scope.pj_class_aliases

let exports_of (scope : project_scope) : exports = scope.pj_exports

let own_bindings ~(classes_by_file : entry list Common.SMap.t)
    ~(class_parent_paths :
        (Function_id.t * IL.name option list) list Common.SMap.t)
    ~(file_funcs_index : (string, Func_info.t list) Hashtbl.t)
    (fi : file_info) : Scope_binding.positioned_binding list =
  let fi_file_str = Fpath.to_string fi.fi_file in
  let namespace_scope_keys =
    List.fold_left
      (fun (keys : unit Common.SMap.t) (namespace_scope : Names.Module_qn.t) ->
        Common.SMap.add (Names.Module_qn.to_string namespace_scope) () keys)
      Common.SMap.empty fi.fi_namespace_scopes
  in
  Scope_binding.own_definitions_of_file ~file_funcs_index ~fi_file_str
  @ Scope_binding.own_alias_bindings ~file_funcs_index ~fi_file_str
  @ Scope_binding.own_class_bindings ~companion:Scope_binding.no_companion
      ~class_parent_paths
      ~binds_at_file_scope:(fun (owner : Names.Class_qn.t) ->
        Common.SMap.mem (Names.Class_qn.to_string owner) namespace_scope_keys)
      ~scope_of_owner:(fun _ -> None)
      (Option.value (Common.SMap.find_opt fi_file_str classes_by_file)
         ~default:[])

let field_alias_target (field : G.field) : (string * string) option =
  match field with
  | G.F { G.s = G.DefStmt ((ent : G.entity),
                           (G.FieldDefColon vd | G.VarDef vd)); _ } -> (
    match (Index_lang_rules.entity_simple_name ent, vd.G.vinit) with
    | Some (alias : string), Some (init : G.expr) ->
      Option.map (fun (target : string) -> (alias, target)) (bare_name_of init)
    | Some _, None
    | None, _ -> None)
  | _ -> None

let class_field_aliases ~(definitions_by_qn : definition Common.SMap.t)
    ~(value_alias_index : (string * string, G.expr) Hashtbl.t)
    ~(objects : Func_info.t list Common.SMap.t Common.SMap.t Common.SMap.t)
    ~(exports : exports)
    ~(classes_by_file : entry list Common.SMap.t) (fi : file_info)
    : (Names.Class_qn.t * string * Func_info.t list) list =
  let own_classes =
    Option.value
      (Common.SMap.find_opt (Fpath.to_string fi.fi_file) classes_by_file)
      ~default:[]
  in
  List.concat_map
    (fun (obs : Walker.Observation.t) ->
      match obs with
      | Walker.Observation.Class_def { ent; cdef } -> (
        match ent.G.name with
        | G.EN (gname : G.name) -> (
          let class_id = Function_id.of_il_name (AST_to_IL.var_of_name gname) in
          match
            List.find_opt
              (fun (ci : entry) -> Function_id.equal ci.id class_id)
              own_classes
          with
          | None -> []
          | Some (ci : entry) ->
            let _, fields, _ = cdef.G.cbody in
            List.filter_map
              (fun (field : G.field) ->
                match field_alias_target field with
                | None -> None
                | Some ((alias : string), (target : string)) -> (
                  match
                    module_level_name ~definitions_by_qn ~value_alias_index
                      ~objects ~exports ~module_qn:fi.fi_module_path fi target
                  with
                  | Some (Exports_definition
                            (Function_definitions (funcs : Func_info.t list))) ->
                    Some ((Scope_binding.class_qn_of_entry ci), alias, funcs)
                  | Some (Exports_definition (Class_definition _))
                  | Some (Exports_object _)
                  | Some (Exports_module _)
                  | None -> None))
              fields)
        | G.EDynamic _
        | G.EPattern _
        | G.OtherEntity _ -> [])
      | _ -> [])
    fi.fi_observations

let build_project_scope ~(definitions_by_qn : definition Common.SMap.t)
    ~(value_alias_index : (string * string, G.expr) Hashtbl.t)
    ~(classes_by_file : entry list Common.SMap.t)
    ~(class_parent_paths :
        (Function_id.t * IL.name option list) list Common.SMap.t)
    ~(file_funcs_index : (string, Func_info.t list) Hashtbl.t)
    ~(file_infos : file_info list) : project_scope =
  let rows = List.map (rows_of_file ~file_funcs_index) file_infos in
  let objects = objects_by_module rows in
  let scripts, script_files =
    List.fold_left
      (fun ((scripts : Func_lookup.scope_entry list Common.SMap.t),
            (script_files : unit Common.SMap.t)) (file : file_rows) ->
        if not (is_script file) then (scripts, script_files)
        else
          ( entries_of_positioned
              (own_bindings ~classes_by_file ~class_parent_paths
                 ~file_funcs_index file.fr_file)
              scripts,
            Common.SMap.add (Fpath.to_string file.fr_file.fi_file) ()
              script_files ))
      (Common.SMap.empty, Common.SMap.empty) rows
  in
  let exports =
    build_exports ~definitions_by_qn ~value_alias_index ~objects rows
  in
  { pj_exports = exports;
    pj_objects = objects;
    pj_scripts = scripts;
    pj_script_files = script_files;
    pj_class_aliases =
      List.concat_map
        (class_field_aliases ~definitions_by_qn ~value_alias_index ~objects
           ~exports ~classes_by_file)
        file_infos }

type file_bindings = {
  fb_scope : Func_lookup.scope_entry list Common.SMap.t;
  fb_module_aliases : (string, Names.Module_qn.t) Hashtbl.t;
  fb_own_modules : Names.Module_qn.t list;
}

let object_binding_of ~(pos : Pos.t option)
    ~(parent_path : IL.name option list) (name : string)
    (members : Func_info.t list Common.SMap.t)
    : Scope_binding.positioned_binding =
  { Scope_binding.pb_pos = pos; pb_name = name; pb_parent_path = parent_path;
    pb_kinds = [ Func_lookup.Scope_object members ] }

let binding_of_export ~(pos : Pos.t option) (local : string)
    (target : export_target)
    : Scope_binding.positioned_binding list
      * (string * Names.Module_qn.t) list =
  match target with
  | Exports_module (module_qn : Names.Module_qn.t) ->
    ([], [ (local, module_qn) ])
  | Exports_object (members : Func_info.t list Common.SMap.t) ->
    ([ object_binding_of ~pos ~parent_path:[] local members ], [])
  | Exports_definition (Function_definitions (funcs : Func_info.t list)) ->
    (Scope_binding.function_binding_of ~pos ~parent_path:[] local funcs, [])
  | Exports_definition (Class_definition { class_qn; _ }) ->
    ([ Scope_binding.class_binding_of ~pos ~parent_path:[] local class_qn ], [])

let alias_rows (fi : file_info) : (string * string * Pos.t option) list =
  List.filter_map
    (fun (stmt : G.stmt) ->
      match stmt.G.s with
      | G.DefStmt ((ent : G.entity), G.VarDef { G.vinit = Some init; _ }) -> (
        match
          (ent.G.name, bare_name_of init)
        with
        | G.EN (G.Id ((name, tok), _)), Some (target : string) ->
          Some (name, target, Scope_binding.position_of_tok tok)
        | _ -> None)
      | G.ExprStmt ({ G.e = G.Assign ({ G.e = G.N (G.Id ((name, tok), _)); _ },
                                      _, init); _ }, _) ->
        Option.map
          (fun (target : string) ->
            (name, target, Scope_binding.position_of_tok tok))
          (bare_name_of init)
      | _ -> None)
    fi.fi_ast

let build ~(scope : project_scope)
    ~(classes_by_file : entry list Common.SMap.t)
    ~(class_parent_paths :
        (Function_id.t * IL.name option list) list Common.SMap.t)
    ~(file_funcs_index : (string, Func_info.t list) Hashtbl.t)
    (fi : file_info) : file_bindings =
  let exports = scope.pj_exports in
  let own_modules =
    List.sort_uniq Names.Module_qn.compare fi.fi_namespace_scopes
  in
  let own =
    own_bindings ~classes_by_file ~class_parent_paths ~file_funcs_index fi
    @ List.concat_map
        (fun (namespace_scope : Names.Module_qn.t) ->
          Common.SMap.fold
            (fun (name : string) (members : Func_info.t list Common.SMap.t)
                 (bindings : Scope_binding.positioned_binding list) ->
              object_binding_of ~pos:None ~parent_path:[] name members
              :: bindings)
            (Option.value
               (Common.SMap.find_opt (Names.Module_qn.to_string namespace_scope)
                  scope.pj_objects)
               ~default:Common.SMap.empty)
            [])
        own_modules
  in
  let imported, aliases =
    List.fold_left
      (fun ((imported : Scope_binding.positioned_binding list),
            (aliases : (string * Names.Module_qn.t) list))
           (imp : import) ->
        match imp.im_role with
        | Role_reexports -> (imported, aliases)
        | Role_binds -> (
          let pos = Scope_binding.position_of_tok imp.im_tok in
          match (Imports.binding_of imp, imp.im_binds) with
          | Imports.Wildcard_from _, _
          | _, Binds_type -> (imported, aliases)
          | Imports.Named_binding { local; target }, Binds_module ->
            let bindings, _ =
              match bound_in exports target "default" with
              | Some (found : export_target) ->
                binding_of_export ~pos local found
              | None -> ([], [])
            in
            (bindings @ imported, (local, target) :: aliases)
          | Imports.Named_binding { local; target },
            (Binds_any | Binds_function | Binds_constant) -> (
            match export_of_target exports target with
            | None -> (imported, aliases)
            | Some (found : export_target) ->
              let bindings, module_aliases =
                binding_of_export ~pos local found
              in
              (bindings @ imported, module_aliases @ aliases))))
      ([], []) fi.fi_imports
  in
  (* Filled in source order, so the head of a dotted target ([Svc] of
     [import S = Svc.Inner]) is read from the aliases that the imports
     above it bound. *)
  let module_aliases : (string, Names.Module_qn.t) Hashtbl.t =
    Hashtbl.create (List.length aliases)
  in
  List.iter
    (fun ((local : string), (target : Names.Module_qn.t)) ->
      Hashtbl.replace module_aliases local
        (match Names.Module_qn.parts target with
         | head :: (_ :: _ as rest) -> (
           match Hashtbl.find_opt module_aliases head with
           | Some (bound : Names.Module_qn.t) ->
             Names.Module_qn.of_parts (Names.Module_qn.parts bound @ rest)
           | None -> target)
         | []
         | [ _ ] -> target))
    (List.rev aliases);
  let bound_before =
    Scope_binding.bindings_of_positioned (own @ List.rev imported)
  in
  let alias_bindings =
    List.filter_map
      (fun (((name : string), (target : string), (pos : Pos.t option))) ->
        if String.equal name target then None
        else
          match Common.SMap.find_opt target bound_before with
          | None -> None
          | Some (entries : Func_lookup.scope_entry list) ->
            Some
              { Scope_binding.pb_pos = pos; pb_name = name;
                pb_parent_path = [];
                pb_kinds =
                  List.map
                    (fun (entry : Func_lookup.scope_entry) ->
                      entry.Func_lookup.kind)
                    entries })
      (alias_rows fi)
  in
  let bound =
    Scope_binding.bindings_of_positioned
      (own @ List.rev imported @ alias_bindings)
  in
  { fb_scope =
      (if Common.SMap.mem (Fpath.to_string fi.fi_file) scope.pj_script_files
       then
         Common.SMap.union
           (fun _ (own : Func_lookup.scope_entry list) _ -> Some own)
           bound scope.pj_scripts
       else bound);
    fb_module_aliases = module_aliases;
    fb_own_modules = own_modules }
