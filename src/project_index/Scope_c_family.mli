val build :
  precedence:(Index_lang_rules.binding_kind -> int) ->
  definitions_by_qn:Types.definition Common.SMap.t ->
  attributes_by_module:Func_lookup.module_attributes ->
  classes_by_file:Types.entry list Common.SMap.t ->
  class_parent_paths:
    (Function_id.t * IL.name option list) list Common.SMap.t ->
  namespace_scope_bindings:Scope_binding.namespace_scope_bindings Common.SMap.t ->
  include_bindings:Scope_binding.positioned_binding list ->
  included_files:string list ->
  file_funcs_index:(string, Func_info.t list) Hashtbl.t ->
  Types.file_info ->
  Func_lookup.scope_entry list Common.SMap.t
