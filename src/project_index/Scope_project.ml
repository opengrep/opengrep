open Types

let build
    ~(classes_by_file : class_info list Common.SMap.t)
    ~(class_parent_paths :
        (Function_id.t * IL.name option list) list Common.SMap.t)
    ~(file_funcs_index : (string, Func_info.t list) Hashtbl.t)
    ~(resolution_orders : Func_lookup.resolution_orders)
    ~(methods_by_class : Func_lookup.methods_by_class)
    ~(top_level_scope : Func_lookup.scope_table)
    (fi : file_info) : Func_lookup.scope_table =
  let fi_file_str = Fpath.to_string fi.fi_file in
  let own_classes =
    Option.value (Common.SMap.find_opt fi_file_str classes_by_file) ~default:[]
  in
  let own_class_by_qn : class_info Common.SMap.t =
    List.fold_left
      (fun (by_qn : class_info Common.SMap.t) (ci : class_info) ->
        Common.SMap.add (Names.Class_qn.to_string ci.ci_qn) ci by_qn)
      Common.SMap.empty own_classes
  in
  let type_bindings =
    Scope_binding.own_class_bindings ~class_parent_paths
      ~binds_at_file_scope:(fun (owner : Names.Class_qn.t) ->
        Names.Class_qn.is_empty owner)
      ~scope_of_owner:(fun (owner : Names.Class_qn.t) ->
        Option.map
          (fun (ci : class_info) ->
            [ Some (Scope_binding.class_il_name_of ci) ])
          (Common.SMap.find_opt (Names.Class_qn.to_string owner)
             own_class_by_qn))
      own_classes
  in
  let function_bindings =
    Scope_binding.own_definitions_of_file ~file_funcs_index ~fi_file_str
  in
  let member_bindings =
    List.concat_map
      (fun (ci : class_info) ->
        let parent_path = [ Some (Scope_binding.class_il_name_of ci) ] in
        let pos = Scope_binding.position_of_tok (Function_id.tok ci.ci_id) in
        List.concat_map
          (fun ((name : string), (funcs : Func_info.t list)) ->
            Scope_binding.function_binding_of ~pos ~parent_path name funcs)
          (Scope_package.members_along_order ~resolution_orders
             ~methods_by_class ci.ci_qn))
      own_classes
  in
  Func_lookup.scope_table_layered
    ~front:
      (Func_lookup.scope_table_of_map
         (Scope_binding.bindings_of_positioned
            (function_bindings @ type_bindings @ member_bindings)))
    ~back:top_level_scope
