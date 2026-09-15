val build :
  tier_rank:(Index_lang_rules.tier -> int) ->
  definitions_by_qn:Types.definition Common.SMap.t ->
  attributes_by_module:Func_lookup.module_attributes ->
  classes_by_file:Types.class_info list Common.SMap.t ->
  class_parent_paths:
    (Function_id.t * IL.name option list) list Common.SMap.t ->
  resolution_orders:Func_lookup.resolution_orders ->
  methods_by_class:Func_lookup.methods_by_class ->
  region_bindings:Scope_binding.region_bindings Common.SMap.t ->
  include_bindings:Scope_binding.positioned_binding list ->
  included_files:string list ->
  file_funcs_index:(string, Func_info.t list) Hashtbl.t ->
  Types.file_info ->
  Func_lookup.scope_entry list Common.SMap.t
