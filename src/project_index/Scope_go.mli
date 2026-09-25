type package_index

val build_package_index :
  cfg:Index_lang_rules.t ->
  file_infos:Types.file_info list ->
  package_index

val importable_clause : package_index -> Names.Module_qn.t -> string option

val import_aliases : Types.file_info -> Names.Module_qn.t Common.SMap.t

val build :
  lang:Lang.t ->
  cfg:Index_lang_rules.t ->
  package_index:package_index ->
  build_constraints:Go_build_constraints.t ->
  attributes_by_module:Func_lookup.module_attributes ->
  classes_by_file:Types.entry list Common.SMap.t ->
  class_parent_paths:
    (Function_id.t * IL.name option list) list Common.SMap.t ->
  file_funcs_index:(string, Func_info.t list) Hashtbl.t ->
  Types.file_info ->
  Func_lookup.scope_entry list Common.SMap.t
  * (string, Names.Module_qn.t) Hashtbl.t
