type region_bindings

val build_region_bindings :
  attributes_by_module:Func_lookup.module_attributes ->
  file_infos:Types.file_info list ->
  region_bindings Common.SMap.t

val global_function_bindings :
  attributes_by_module:Func_lookup.module_attributes ->
  Scope_binding.positioned_binding list

val build :
  definitions_by_qn:Types.definition Common.SMap.t ->
  region_bindings:region_bindings Common.SMap.t ->
  global_bindings:Scope_binding.positioned_binding list ->
  classes_by_file:Types.class_info list Common.SMap.t ->
  class_parent_paths:
    (Function_id.t * IL.name option list) list Common.SMap.t ->
  file_funcs_index:(string, Func_info.t list) Hashtbl.t ->
  Types.file_info ->
  Func_lookup.scope_entry list Common.SMap.t * Names.Module_qn.t list
