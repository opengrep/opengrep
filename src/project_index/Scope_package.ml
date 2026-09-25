open Types

type binding_with_kind = {
  kind : Index_lang_rules.binding_kind;
  binding : Scope_binding.positioned_binding;
}

let of_kind (kind : Index_lang_rules.binding_kind)
    (bindings : Scope_binding.positioned_binding list)
    : binding_with_kind list =
  List.map
    (fun (binding : Scope_binding.positioned_binding) -> { kind; binding })
    bindings

let unambiguous_on_demand
    (bindings : Scope_binding.positioned_binding list)
    : Scope_binding.positioned_binding list =
  let sources =
    List.fold_left
      (fun (sources : Pos.t option list Common.SMap.t)
           (binding : Scope_binding.positioned_binding) ->
        Common.SMap.update binding.Scope_binding.pb_name
          (function
            | None -> Some [ binding.Scope_binding.pb_pos ]
            | Some (earlier : Pos.t option list) ->
              Some (binding.Scope_binding.pb_pos :: earlier))
          sources)
      Common.SMap.empty bindings
  in
  List.filter
    (fun (binding : Scope_binding.positioned_binding) ->
      match Common.SMap.find_opt binding.Scope_binding.pb_name sources with
      | None -> true
      | Some (positions : Pos.t option list) ->
        Int.equal
          (List.length (List.sort_uniq (Option.compare Pos.compare) positions))
          1)
    bindings

let is_extension_binding (binding : Scope_binding.positioned_binding) : bool =
  List.for_all
    (fun (kind : Func_lookup.scope_kind) ->
      match kind with
      | Func_lookup.Scope_extension _ -> true
      | Func_lookup.Scope_function _
      | Func_lookup.Scope_object _
      | Func_lookup.Scope_local_value
      | Func_lookup.Scope_class _
      | Func_lookup.Scope_companion _ -> false)
    binding.Scope_binding.pb_kinds

let keep_highest_precedence ~(precedence : Index_lang_rules.binding_kind -> int)
    (bindings : binding_with_kind list)
    : Scope_binding.positioned_binding list =
  let at_file_scope (entry : binding_with_kind) : bool =
    (match entry.binding.Scope_binding.pb_parent_path with
     | [] -> true
     | _ :: _ -> false)
    && not (is_extension_binding entry.binding)
  in
  let precedence_of (entry : binding_with_kind) : int = precedence entry.kind in
  let highest =
    List.fold_left
      (fun (highest : int Common.SMap.t) (entry : binding_with_kind) ->
        if not (at_file_scope entry) then highest
        else
          let name = entry.binding.Scope_binding.pb_name in
          Common.SMap.add name (precedence_of entry) highest)
      Common.SMap.empty
      (List.filter at_file_scope bindings
       |> List.stable_sort (fun (first : binding_with_kind) (second : binding_with_kind) ->
              Int.compare (precedence_of first) (precedence_of second)))
  in
  List.filter_map
    (fun (entry : binding_with_kind) ->
      if not (at_file_scope entry) then Some entry.binding
      else
        match
          Common.SMap.find_opt entry.binding.Scope_binding.pb_name highest
        with
        | Some (winner : int) when Int.equal winner (precedence_of entry) ->
          Some entry.binding
        | Some _
        | None -> None)
    bindings

let build
    ~(precedence : Index_lang_rules.binding_kind -> int)
    ~(own_package_members_kind : Index_lang_rules.binding_kind)
    ~(namespaces_nest : bool)
    ~(definitions_by_qn : definition Common.SMap.t)
    ~(attributes_by_module : Func_lookup.module_attributes)
    ~(classes_by_file : entry list Common.SMap.t)
    ~(class_parent_paths :
        (Function_id.t * IL.name option list) list Common.SMap.t)
    ~(file_funcs_index : (string, Func_info.t list) Hashtbl.t)
    ~(extensions_by_module : Func_info.t list Common.SMap.t Common.SMap.t)
    ~(nested_types_by_class : Names.Class_qn.t Common.SMap.t Common.SMap.t)
    ~(global_imports : import list)
    ~(object_classes : unit Common.SMap.t)
    ~(companions : bool)
    (fi : file_info)
    : Func_lookup.scope_entry list Common.SMap.t
      * Names.Class_qn.t list =
  let fi_file_str = Fpath.to_string fi.fi_file in
  let own_classes =
    Option.value (Common.SMap.find_opt fi_file_str classes_by_file) ~default:[]
  in
  let namespace_scopes : unit Common.SMap.t =
    List.fold_left
      (fun (namespace_scopes : unit Common.SMap.t) (namespace_scope : Names.Module_qn.t) ->
        Common.SMap.add (Names.Module_qn.to_string namespace_scope) () namespace_scopes)
      Common.SMap.empty fi.fi_namespace_scopes
  in
  let own_class_by_qn : entry Common.SMap.t =
    List.fold_left
      (fun (by_qn : entry Common.SMap.t) (ci : entry) ->
        Common.SMap.add (Names.Class_qn.to_string (Scope_binding.class_qn_of_entry ci)) ci by_qn)
      Common.SMap.empty own_classes
  in
  let type_bindings =
    Scope_binding.own_class_bindings
      ~companion:(fun (ci : entry) ->
        companions
        && (match ci.kind with
            | K_companion -> true
            | K_class
            | K_function
            | K_method -> false))
      ~class_parent_paths
      ~binds_at_file_scope:(fun (owner : Names.Class_qn.t) ->
        Common.SMap.mem (Names.Class_qn.to_string owner) namespace_scopes)
      ~scope_of_owner:(fun (owner : Names.Class_qn.t) ->
        Option.map
          (fun (ci : entry) ->
            [ Some (Scope_binding.class_il_name_of ci) ])
          (Common.SMap.find_opt (Names.Class_qn.to_string owner)
             own_class_by_qn))
      own_classes
  in
  let function_bindings =
    Scope_binding.own_definitions_of_file ~file_funcs_index ~fi_file_str
  in
  let alias_bindings =
    Scope_binding.own_alias_bindings ~file_funcs_index ~fi_file_str
  in
  let bindings_of_module ~(pos : Pos.t option) (target : Names.Module_qn.t)
      : Scope_binding.positioned_binding list =
    Scope_binding.bindings_of_every_attribute ~pos
      (Func_lookup.attributes_of_module attributes_by_module target)
  in
  let objects_of_module (target : Names.Module_qn.t) : Names.Class_qn.t list =
    Common.SMap.fold
      (fun (_ : string) (attribute : Func_lookup.module_attribute)
           (objects : Names.Class_qn.t list) ->
        match attribute with
        | Func_lookup.Attr_class (class_qn : Names.Class_qn.t)
          when Common.SMap.mem (Names.Class_qn.to_string class_qn)
                 object_classes ->
          class_qn :: objects
        | Func_lookup.Attr_class _
        | Func_lookup.Attr_class_with_companion _
        | Func_lookup.Attr_functions _
        | Func_lookup.Attr_module _ -> objects)
      (Func_lookup.attributes_of_module attributes_by_module target)
      []
  in
  let bindings_of_nested_types ~(pos : Pos.t option)
      (class_qn : Names.Class_qn.t)
      : Scope_binding.positioned_binding list =
    Common.SMap.fold
      (fun (name : string) (nested_qn : Names.Class_qn.t)
           (bindings : Scope_binding.positioned_binding list) ->
        Scope_binding.class_binding_of ~pos ~parent_path:[] name nested_qn
        :: bindings)
      (Option.value
         (Common.SMap.find_opt (Names.Class_qn.to_string class_qn)
            nested_types_by_class)
         ~default:Common.SMap.empty)
      []
  in
  let class_target (target : Names.Module_qn.t) : Names.Class_qn.t option =
    match
      Common.SMap.find_opt (Names.Module_qn.to_string target) definitions_by_qn
    with
    | Some (Class_definition { class_qn; _ }) -> Some class_qn
    | Some (Function_definitions _)
    | None -> None
  in
  let on_demand_bindings (imp : import) (target : Names.Module_qn.t)
      : Scope_binding.positioned_binding list =
    let bindings =
      match class_target target with
      | Some _ when imp.im_static -> []
      | Some (class_qn : Names.Class_qn.t) ->
        bindings_of_nested_types
          ~pos:(Scope_binding.position_of_tok imp.im_tok) class_qn
      | None ->
        bindings_of_module ~pos:(Scope_binding.position_of_tok imp.im_tok)
          target
    in
    match imp.im_hidden with
    | [] -> bindings
    | (_ :: _) as hidden ->
      List.filter
        (fun (binding : Scope_binding.positioned_binding) ->
          not (List.exists (String.equal binding.Scope_binding.pb_name) hidden))
        bindings
  in
  let imported, on_demand =
    List.fold_left
      (fun ((imported : Scope_binding.positioned_binding list),
            (on_demand : Scope_binding.positioned_binding list))
           (imp : import) ->
        match Imports.binding_of imp with
        | Imports.Wildcard_from (target : Names.Module_qn.t) ->
          (imported, on_demand_bindings imp target @ on_demand)
        | Imports.Named_binding { local; target } -> (
          match
            Common.SMap.find_opt (Names.Module_qn.to_string target)
              definitions_by_qn
          with
          | Some (Function_definitions (funcs : Func_info.t list)) ->
            ( Scope_binding.function_binding_of ~pos:(Scope_binding.position_of_tok imp.im_tok) ~parent_path:[]
                local funcs
              @ imported,
              on_demand )
          | Some (Class_definition { class_qn; class_companion; _ }) ->
            ( Scope_binding.class_binding_of ~pos:(Scope_binding.position_of_tok imp.im_tok) ~parent_path:[]
                local class_qn
              :: (match class_companion with
                  | Some (companion_qn : Names.Class_qn.t) ->
                    [ Scope_binding.companion_binding_of
                        ~pos:(Scope_binding.position_of_tok imp.im_tok)
                        ~parent_path:[] local companion_qn ]
                  | None -> [])
              @ imported,
              on_demand )
          | None -> (imported, on_demand)))
      ([], []) fi.fi_imports
  in
  let global_bindings =
    List.concat_map
      (fun (imp : import) ->
        match Imports.binding_of imp with
        | Imports.Wildcard_from (target : Names.Module_qn.t) ->
          on_demand_bindings imp target
        | Imports.Named_binding _ -> [])
      global_imports
  in
  let rec enclosing_namespaces (namespace_scope : Names.Module_qn.t)
      : Names.Module_qn.t list =
    match Names.Module_qn.split_last namespace_scope with
    | None -> []
    | Some ((enclosing : Names.Module_qn.t), _) ->
      enclosing :: enclosing_namespaces enclosing
  in
  let own_namespaces : Names.Module_qn.t list =
    (if namespaces_nest then
       List.concat_map
         (fun (namespace_scope : Names.Module_qn.t) ->
           namespace_scope :: enclosing_namespaces namespace_scope)
         fi.fi_namespace_scopes
     else fi.fi_namespace_scopes)
    |> List.sort_uniq Names.Module_qn.compare
  in
  let own_namespace_scope_bindings =
    List.concat_map
      (fun (namespace_scope : Names.Module_qn.t) -> bindings_of_module ~pos:None namespace_scope)
      own_namespaces
  in
  let extension_namespaces : Names.Module_qn.t list =
    own_namespaces
    @ List.filter_map
        (fun (imp : import) ->
          match Imports.binding_of imp with
          | Imports.Wildcard_from (target : Names.Module_qn.t)
            when Option.is_none (class_target target) -> Some target
          | Imports.Wildcard_from _
          | Imports.Named_binding _ -> None)
        (fi.fi_imports @ global_imports)
    |> List.sort_uniq Names.Module_qn.compare
  in
  let extension_bindings =
    List.concat_map
      (fun (namespace : Names.Module_qn.t) ->
        Common.SMap.fold
          (fun (name : string) (funcs : Func_info.t list)
               (bindings : Scope_binding.positioned_binding list) ->
            { Scope_binding.pb_pos = None; pb_name = name; pb_parent_path = [];
              pb_kinds =
                List.map
                  (fun (func : Func_info.t) ->
                    Func_lookup.Scope_extension func)
                  funcs }
            :: bindings)
          (Option.value
             (Common.SMap.find_opt (Names.Module_qn.to_string namespace)
                extensions_by_module)
             ~default:Common.SMap.empty)
          [])
      extension_namespaces
  in
  let bindings =
    of_kind Index_lang_rules.Wildcard_import
      (unambiguous_on_demand (global_bindings @ List.rev on_demand)
       @ extension_bindings)
    @ of_kind own_package_members_kind own_namespace_scope_bindings
    @ of_kind Index_lang_rules.Own_definition
        (function_bindings @ alias_bindings @ type_bindings)
    @ of_kind Index_lang_rules.Single_import (List.rev imported)
  in
  let wildcard_targets (static : bool) : Names.Module_qn.t list =
    List.filter_map
      (fun (imp : import) ->
        match Imports.binding_of imp with
        | Imports.Wildcard_from (target : Names.Module_qn.t)
          when Bool.equal imp.im_static static -> Some target
        | Imports.Wildcard_from _
        | Imports.Named_binding _ -> None)
      (fi.fi_imports @ global_imports)
  in
  let member_classes =
    List.filter_map class_target (wildcard_targets true)
    @ List.concat_map objects_of_module
        (own_namespaces @ wildcard_targets false)
  in
  (Scope_binding.bindings_of_positioned (keep_highest_precedence ~precedence bindings),
   member_classes)
