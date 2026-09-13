val top_level_bindings :
  definitions_by_qn:Types.definition Common.SMap.t ->
  Scope_binding.positioned_binding list

val build :
  classes_by_file:Types.class_info list Common.SMap.t ->
  class_parent_paths:
    (Function_id.t * IL.name option list) list Common.SMap.t ->
  file_funcs_index:(string, Func_info.t list) Hashtbl.t ->
  resolution_orders:Func_lookup.resolution_orders ->
  methods_by_class:Func_lookup.methods_by_class ->
  nested_types_by_class:Names.Class_qn.t Common.SMap.t Common.SMap.t ->
  Types.file_info ->
  Func_lookup.scope_entry list Common.SMap.t * Names.Module_qn.t list
