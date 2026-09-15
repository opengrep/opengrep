type tier =
  | On_demand
  | Single_import
  | Own_scope
  | Package_members

type tiered = {
  tier : tier;
  binding : Scope_binding.positioned_binding;
}

val at_tier : tier -> Scope_binding.positioned_binding list -> tiered list

val unambiguous_on_demand :
  Scope_binding.positioned_binding list ->
  Scope_binding.positioned_binding list

val keep_strongest :
  Lang.t -> tiered list -> Scope_binding.positioned_binding list

val members_along_order :
  resolution_orders:Func_lookup.resolution_orders ->
  methods_by_class:Func_lookup.methods_by_class ->
  Names.Class_qn.t ->
  (string * Func_info.t list) list

val build :
  lang:Lang.t ->
  definitions_by_qn:Types.definition Common.SMap.t ->
  attributes_by_module:Func_lookup.module_attributes ->
  classes_by_file:Types.class_info list Common.SMap.t ->
  class_parent_paths:
    (Function_id.t * IL.name option list) list Common.SMap.t ->
  file_funcs_index:(string, Func_info.t list) Hashtbl.t ->
  resolution_orders:Func_lookup.resolution_orders ->
  methods_by_class:Func_lookup.methods_by_class ->
  extensions_by_module:Func_info.t list Common.SMap.t Common.SMap.t ->
  nested_types_by_class:Names.Class_qn.t Common.SMap.t Common.SMap.t ->
  global_imports:Types.import list ->
  namespace_object_members:Scope_binding.positioned_binding list Common.SMap.t ->
  companions:bool ->
  Types.file_info ->
  Func_lookup.scope_entry list Common.SMap.t
  * (Names.Class_name.t * Fpath.t) list
