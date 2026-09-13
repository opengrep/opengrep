val build :
  include_bindings:Scope_binding.positioned_binding list ->
  file_funcs_index:(string, Func_info.t list) Hashtbl.t ->
  Types.file_info ->
  Func_lookup.scope_entry list Common.SMap.t
