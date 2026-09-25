val build :
  cfg:Index_lang_rules.t ->
  definitions_by_qn:Types.definition Common.SMap.t ->
  attributes_by_module:Func_lookup.module_attributes ->
  classes_by_file:Types.entry list Common.SMap.t ->
  class_parent_paths:
    (Function_id.t * IL.name option list) list Common.SMap.t ->
  file_funcs_index:(string, Func_info.t list) Hashtbl.t ->
  import_aliases:(string, Names.Module_qn.t) Hashtbl.t option ->
  Types.file_info ->
  Func_lookup.scope_entry list Common.SMap.t
  * (string, Names.Module_qn.t) Hashtbl.t
