module G = AST_generic
module FA = Graph_from_AST

open Types

let init_function_name : string = "init"

let blank_identifier : string = "_"

type package_index = {
  pi_files : Fpath.t list Common.SMap.t Common.SMap.t;
  pi_importable_clause : string Common.SMap.t;
}

let is_external_test_clause (clauses : string list) (clause : string) : bool =
  List.exists
    (fun (other : string) ->
      (not (String.equal other clause))
      && String.equal clause (other ^ "_test"))
    clauses

let importable_clause (by_clause : Fpath.t list Common.SMap.t)
    : string option =
  let clauses = List.map fst (Common.SMap.bindings by_clause) in
  List.find_opt
    (fun (clause : string) -> not (is_external_test_clause clauses clause))
    clauses

let empty_package_index : package_index =
  { pi_files = Common.SMap.empty; pi_importable_clause = Common.SMap.empty }

let go_package_index ~(file_infos : file_info list) : package_index =
  let pi_files =
    List.fold_left
      (fun (by_path : Fpath.t list Common.SMap.t Common.SMap.t)
           (fi : file_info) ->
        match fi.fi_package_clause with
        | None -> by_path
        | Some (clause : string) ->
          let path = Names.Module_qn.to_string fi.fi_module_path in
          let by_clause =
            Option.value (Common.SMap.find_opt path by_path)
              ~default:Common.SMap.empty
          in
          let files =
            Option.value (Common.SMap.find_opt clause by_clause) ~default:[]
          in
          Common.SMap.add path
            (Common.SMap.add clause (fi.fi_file :: files) by_clause)
            by_path)
      Common.SMap.empty file_infos
  in
  { pi_files;
    pi_importable_clause =
      Common.SMap.filter_map
        (fun (_ : string) (by_clause : Fpath.t list Common.SMap.t) ->
          importable_clause by_clause)
        pi_files }

let build_package_index ~(cfg : Index_lang_rules.t)
    ~(file_infos : file_info list) : package_index =
  match cfg.Index_lang_rules.unqualified_scope with
  | `Per_file
  | `Per_constant_path
  | `Per_directory
  | `Per_module
  | `Per_namespace
  | `Per_package -> empty_package_index
  | `Per_go_package -> go_package_index ~file_infos

let importable_clause (package_index : package_index)
    (module_qn : Names.Module_qn.t) : string option =
  Common.SMap.find_opt (Names.Module_qn.to_string module_qn)
    package_index.pi_importable_clause

let files_of_package (package_index : package_index) (fi : file_info)
    : Fpath.t list =
  match fi.fi_package_clause with
  | None -> [ fi.fi_file ]
  | Some (clause : string) ->
    Option.value
      (Option.bind
         (Common.SMap.find_opt
            (Names.Module_qn.to_string fi.fi_module_path)
            package_index.pi_files)
         (Common.SMap.find_opt clause))
      ~default:[ fi.fi_file ]

let local_name_of_import (imp : import) : string option =
  match imp.im_alias with
  | Some (alias : string) when String.equal alias blank_identifier -> None
  | Some _
  | None -> Some imp.im_local

let import_aliases (fi : file_info) : Names.Module_qn.t Common.SMap.t =
  List.fold_left
    (fun (aliases : Names.Module_qn.t Common.SMap.t) (imp : import) ->
      match Imports.binding_of imp with
      | Imports.Wildcard_from _ -> aliases
      | Imports.Named_binding _ -> (
        match local_name_of_import imp with
        | None -> aliases
        | Some (local : string) -> Common.SMap.add local imp.im_target aliases))
    Common.SMap.empty fi.fi_imports

let package_bindings
    ~(classes_by_file : class_info list Common.SMap.t)
    ~(class_parent_paths :
        (Function_id.t * IL.name option list) list Common.SMap.t)
    ~(file_funcs_index : (string, Func_info.t list) Hashtbl.t)
    ~(package_qn : Names.Module_qn.t)
    (files : Fpath.t list) : Scope_binding.positioned_binding list =
  List.concat_map
    (fun (file : Fpath.t) ->
      let fi_file_str = Fpath.to_string file in
      Scope_binding.own_definitions_of_file ~file_funcs_index ~fi_file_str
      @ Scope_binding.own_alias_bindings ~file_funcs_index ~fi_file_str
      @ Scope_binding.own_class_bindings ~class_parent_paths
          ~binds_at_file_scope:(fun (owner : Names.Class_qn.t) ->
            String.equal (Names.Class_qn.to_string owner)
              (Names.Module_qn.to_string package_qn))
          ~scope_of_owner:(fun _ -> None)
          (Option.value (Common.SMap.find_opt fi_file_str classes_by_file)
             ~default:[]))
    files
  |> List.filter
       (fun (binding : Scope_binding.positioned_binding) ->
         not (String.equal binding.Scope_binding.pb_name init_function_name))

let dot_import_bindings
    ~(cfg : Index_lang_rules.t)
    ~(attributes_by_module : Func_lookup.module_attributes)
    ~(bound_by_package : string -> bool)
    (target : Names.Module_qn.t) (pos : Pos.t option)
    : Scope_binding.positioned_binding list =
  Scope_binding.bindings_of_attributes ~pos
    ~keep:(fun (name : string) (_ : Func_lookup.module_attribute) ->
      cfg.Index_lang_rules.name_is_exported name
      && not (bound_by_package name))
    (Func_lookup.attributes_of_module attributes_by_module target)

let class_of_entries (bound : Func_lookup.scope_entry list Common.SMap.t)
    (name : string) : Names.Class_qn.t option =
  Func_lookup.class_of_entries
    (Option.value (Common.SMap.find_opt name bound) ~default:[])

let functions_of_entries (bound : Func_lookup.scope_entry list Common.SMap.t)
    (name : string) : Func_info.t list =
  Func_lookup.functions_of_entries
    (Option.value (Common.SMap.find_opt name bound) ~default:[])

let class_qn_of_module_attribute
    ~(attributes_by_module : Func_lookup.module_attributes)
    (target : Names.Module_qn.t) (name : string) : Names.Class_qn.t option =
  match
    Common.SMap.find_opt name
      (Func_lookup.attributes_of_module attributes_by_module target)
  with
  | Some (Func_lookup.Attr_class (class_qn : Names.Class_qn.t)) -> Some class_qn
  | Some (Func_lookup.Attr_functions _)
  | Some (Func_lookup.Attr_module _)
  | None -> None

let alias_bindings
    ~(attributes_by_module : Func_lookup.module_attributes)
    ~(module_aliases : (string, Names.Module_qn.t) Hashtbl.t)
    ~(bound_by_package : Func_lookup.scope_entry list Common.SMap.t)
    (fi : file_info) : Scope_binding.positioned_binding list =
  List.filter_map
    (fun (observation : Walker.Observation.t) ->
      match observation with
      | Walker.Observation.Type_def
          { ent; tdef = { G.tbody = G.AliasType (ty : G.type_) } } -> (
        match Index_lang_rules.entity_simple_name ent with
        | None -> None
        | Some (alias : string) ->
          let pos =
            match ent.G.name with
            | G.EN (G.Id ((_, tok), _)) -> Scope_binding.position_of_tok tok
            | _ -> None
          in
          let target_qn =
            match Ty_bare_name.qualified_class_name_of_ty ty with
            | None -> None
            | Some (name : G.name) -> (
              match (Ty_bare_name.qualifier_of_name name,
                     Ty_bare_name.bare_name_of_name name) with
              | _, None -> None
              | None, Some (bare_name : string) ->
                class_of_entries bound_by_package bare_name
              | Some (qualifier : string), Some (bare_name : string) ->
                Option.bind (Hashtbl.find_opt module_aliases qualifier)
                  (fun (target : Names.Module_qn.t) ->
                    class_qn_of_module_attribute ~attributes_by_module target
                      bare_name))
          in
          Option.map
            (fun (class_qn : Names.Class_qn.t) ->
              Scope_binding.class_binding_of ~pos ~parent_path:[] alias
                class_qn)
            target_qn)
      | Walker.Observation.Type_def _
      | Walker.Observation.Func_def _
      | Walker.Observation.Class_def _
      | Walker.Observation.Other_def _
      | Walker.Observation.Var_def _ -> None)
    fi.fi_observations

let declared_in_body
    ~(bound_by_package : Func_lookup.scope_entry list Common.SMap.t)
    ~(parent_path : IL.name option list) (body : G.stmt)
    : Scope_binding.positioned_binding list =
  let rec value_of (rhs : G.expr) : string option =
    match rhs.G.e with
    | G.N (name : G.name) -> Ty_bare_name.bare_name_of_name name
    | G.ArrayAccess ((head : G.expr), _) -> value_of head
    | _ -> None
  in
  let bind ((declared : string), (tok : Tok.t)) (rhs : G.expr)
      : Scope_binding.positioned_binding list =
    if String.equal declared blank_identifier then []
    else
      let pos = Scope_binding.position_of_tok tok in
      match
        Option.map (functions_of_entries bound_by_package) (value_of rhs)
      with
      | Some ((_ :: _) as funcs) ->
        Scope_binding.function_binding_of ~pos ~parent_path declared funcs
      | Some []
      | None ->
        [ { Scope_binding.pb_pos = pos; pb_name = declared;
            pb_parent_path = parent_path;
            pb_kinds = [ Func_lookup.Scope_local_value ] } ]
  in
  let of_target (lhs : G.expr) (rhs : G.expr)
      : Scope_binding.positioned_binding list =
    match lhs.G.e with
    | G.N (G.Id ((declared : G.ident), _)) -> bind declared rhs
    | _ -> []
  in
  Walker.fold_stmts_in_stmt ~skip_nested_fdefs:true
    (fun (bindings : Scope_binding.positioned_binding list) (stmt : G.stmt) ->
      match stmt.G.s with
      | G.ExprStmt ({ G.e = G.Assign (lhs, _, rhs); _ }, _)
      | G.ExprStmt ({ G.e = G.AssignOp (lhs, (G.Eq, _), rhs); _ }, _) ->
        of_target lhs rhs @ bindings
      | G.DefStmt ({ G.name = G.EN (G.Id ((declared : G.ident), _)); _ },
                   G.VarDef { G.vinit = Some (rhs : G.expr); _ }) ->
        bind declared rhs @ bindings
      | _ -> bindings)
    [] body

let body_bindings ~(lang : Lang.t)
    ~(bound_by_package : Func_lookup.scope_entry list Common.SMap.t)
    (fi : file_info) : Scope_binding.positioned_binding list =
  List.concat_map
    (fun (observation : Walker.Observation.t) ->
      match observation with
      | Walker.Observation.Func_def { opt_ent; parent_path; fdef } -> (
        match FA.fn_id_of_entity ~lang opt_ent parent_path fdef with
        | None -> []
        | Some (fn_id : FA.fn_id) ->
          declared_in_body ~bound_by_package ~parent_path:fn_id
            (AST_generic_helpers.funcbody_to_stmt fdef.G.fbody))
      | Walker.Observation.Type_def _
      | Walker.Observation.Class_def _
      | Walker.Observation.Other_def _
      | Walker.Observation.Var_def _ -> [])
    fi.fi_observations

let build ~(lang : Lang.t) ~(cfg : Index_lang_rules.t)
    ~(package_index : package_index)
    ~(attributes_by_module : Func_lookup.module_attributes)
    ~(classes_by_file : class_info list Common.SMap.t)
    ~(class_parent_paths :
        (Function_id.t * IL.name option list) list Common.SMap.t)
    ~(file_funcs_index : (string, Func_info.t list) Hashtbl.t)
    (fi : file_info)
    : Func_lookup.scope_entry list Common.SMap.t
      * (string, Names.Module_qn.t) Hashtbl.t =
  let in_package =
    package_bindings ~classes_by_file ~class_parent_paths ~file_funcs_index
      ~package_qn:fi.fi_module_path
      (files_of_package package_index fi)
  in
  let bound_by_package = Scope_binding.bindings_of_positioned in_package in
  let module_aliases : (string, Names.Module_qn.t) Hashtbl.t =
    Hashtbl.create (List.length fi.fi_imports)
  in
  Common.SMap.iter
    (fun (local : string) (target : Names.Module_qn.t) ->
      Hashtbl.replace module_aliases local target)
    (import_aliases fi);
  let imported =
    List.concat_map
      (fun (imp : import) ->
        match Imports.binding_of imp with
        | Imports.Wildcard_from (target : Names.Module_qn.t) ->
          dot_import_bindings ~cfg ~attributes_by_module
            ~bound_by_package:(fun (name : string) ->
              Common.SMap.mem name bound_by_package)
            target (Scope_binding.position_of_tok imp.im_tok)
        | Imports.Named_binding _ -> [])
      fi.fi_imports
  in
  let aliases =
    alias_bindings ~attributes_by_module ~module_aliases ~bound_by_package fi
  in
  let in_body = body_bindings ~lang ~bound_by_package fi in
  ( Scope_binding.bindings_of_positioned
      (imported @ in_package @ aliases @ in_body),
    module_aliases )
