val build :
  classes_by_file:Types.class_info list Common.SMap.t ->
  class_parent_paths:
    (Function_id.t * IL.name option list) list Common.SMap.t ->
  file_funcs_index:(string, Func_info.t list) Hashtbl.t ->
  resolution_orders:Func_lookup.resolution_orders ->
  methods_by_class:Func_lookup.methods_by_class ->
  module_object_by_module:Names.Class_qn.t Common.SMap.t ->
  top_level_scope:Func_lookup.scope_table ->
  Types.file_info ->
  Func_lookup.scope_table
