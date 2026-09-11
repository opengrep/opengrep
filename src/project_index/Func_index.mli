(* Bucket free funcs by def-file directory basename; empty unless [`Per_directory]. *)
val build_by_package :
  cfg:Index_lang_rules.t ->
  Graph_from_AST.func_info list ->
  (string, Graph_from_AST.func_info list) Hashtbl.t

val build_attributes_by_module :
  cfg:Index_lang_rules.t ->
  dunder_all:(string, unit) Hashtbl.t Common.SMap.t ->
  definitions_by_qn:Types.definition Common.SMap.t ->
  file_infos:Types.file_info list ->
  Func_lookup.module_attributes

(* Bucket free funcs by file [Module_qn]; empty unless the scope is
   [`Per_file] or [`Per_directory]. *)
val build_by_module :
  cfg:Index_lang_rules.t ->
  file_infos:Types.file_info list ->
  Graph_from_AST.func_info list ->
  (Names.Module_qn.t, Graph_from_AST.func_info list) Hashtbl.t

val build_extensions_by_module :
  definitions_by_qn:Types.definition Common.SMap.t ->
  Func_info.t list Common.SMap.t Common.SMap.t

val build_nested_types_by_class :
  definitions_by_qn:Types.definition Common.SMap.t ->
  Names.Class_qn.t Common.SMap.t Common.SMap.t
