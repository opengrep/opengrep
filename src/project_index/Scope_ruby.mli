val build :
  classes_by_file:Types.entry list Common.SMap.t ->
  class_parent_paths:
    (Function_id.t * IL.name option list) list Common.SMap.t ->
  file_funcs_index:(string, Func_info.t list) Hashtbl.t ->
  nested_types_by_class:Names.Class_qn.t Common.SMap.t Common.SMap.t ->
  Types.file_info ->
  Func_lookup.scope_entry list Common.SMap.t * Names.Module_qn.t list
