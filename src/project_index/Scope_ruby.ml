open Types

let top_level_bindings ~(definitions_by_qn : definition Common.SMap.t)
    : Scope_binding.positioned_binding list =
  Common.SMap.fold
    (fun (qn : string) (definition : definition)
         (bindings : Scope_binding.positioned_binding list) ->
      match Names.Def_qn.split_last (Names.Def_qn.of_string qn) with
      | Some ((owner : Names.Def_qn.t), (name : string))
        when Names.Def_qn.is_empty owner -> (
        match definition with
        | Function_definitions (funcs : Func_info.t list) ->
          Scope_binding.function_binding_of ~pos:None ~parent_path:[] name funcs
          @ bindings
        | Class_definition { class_qn; _ } ->
          Scope_binding.class_binding_of ~pos:None ~parent_path:[] name class_qn
          :: bindings)
      | Some _
      | None -> bindings)
    definitions_by_qn []

let own_regions (classes : class_info list) : Names.Module_qn.t list =
  List.sort_uniq Names.Module_qn.compare
    (List.map
       (fun (ci : class_info) ->
         Names.Module_qn.of_string (Names.Class_qn.to_string ci.ci_qn))
       classes)

let build
    ~(classes_by_file : class_info list Common.SMap.t)
    ~(class_parent_paths :
        (Function_id.t * IL.name option list) list Common.SMap.t)
    ~(file_funcs_index : (string, Func_info.t list) Hashtbl.t)
    ~(resolution_orders : Func_lookup.resolution_orders)
    ~(methods_by_class : Func_lookup.methods_by_class)
    ~(nested_types_by_class : Names.Class_qn.t Common.SMap.t Common.SMap.t)
    (fi : file_info)
    : Func_lookup.scope_entry list Common.SMap.t * Names.Module_qn.t list =
  let fi_file_str = Fpath.to_string fi.fi_file in
  let own_classes =
    Option.value (Common.SMap.find_opt fi_file_str classes_by_file) ~default:[]
  in
  let own_by_qn = Scope_binding.classes_by_qn own_classes in
  let function_bindings =
    Scope_binding.own_definitions_of_file ~file_funcs_index ~fi_file_str
  in
  let alias_bindings =
    Scope_binding.own_alias_bindings ~file_funcs_index ~fi_file_str
  in
  let type_bindings =
    Scope_binding.own_class_bindings ~class_parent_paths
      ~binds_at_file_scope:Names.Class_qn.is_empty
      ~scope_of_owner:(fun (owner : Names.Class_qn.t) ->
        Option.map
          (fun (ci : class_info) ->
            [ Some (Scope_binding.class_il_name_of ci) ])
          (Common.SMap.find_opt (Names.Class_qn.to_string owner) own_by_qn))
      own_classes
  in
  let member_bindings =
    Scope_binding.class_member_bindings
      ~members_of:
        (Scope_package.members_along_order ~resolution_orders
           ~methods_by_class)
      own_classes
  in
  let nesting_bindings =
    List.concat_map
      (fun (ci : class_info) ->
        Scope_binding.bindings_in_class ci (fun ~pos ~parent_path ->
          List.concat_map
            (fun (prefix : Names.Class_qn.t) ->
              Common.SMap.fold
                (fun (name : string) (nested_qn : Names.Class_qn.t)
                     (bindings : Scope_binding.positioned_binding list) ->
                  Scope_binding.class_binding_of ~pos ~parent_path name nested_qn
                  :: bindings)
                (Option.value
                   (Common.SMap.find_opt (Names.Class_qn.to_string prefix)
                      nested_types_by_class)
                   ~default:Common.SMap.empty)
                [])
            (match Names.Class_qn.prefixes ci.ci_qn with
             | [] -> []
             | _ :: (proper : Names.Class_qn.t list) -> proper)))
      own_classes
  in
  ( Scope_binding.bindings_of_positioned
      (nesting_bindings @ member_bindings @ type_bindings @ function_bindings
       @ alias_bindings),
    own_regions own_classes )
