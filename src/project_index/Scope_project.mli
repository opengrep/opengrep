val build :
  classes_by_file:Types.entry list Common.SMap.t ->
  class_parent_paths:
    (Function_id.t * IL.name option list) list Common.SMap.t ->
  file_funcs_index:(string, Func_info.t list) Hashtbl.t ->
  module_object_by_module:Names.Class_qn.t Common.SMap.t ->
  top_level_scope:Func_lookup.scope_table ->
  Types.file_info ->
  Func_lookup.scope_table
