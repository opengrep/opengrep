(* Bucket free funcs by def-file directory basename; empty unless [`Per_directory]. *)
val build_by_package :
  cfg:Index_lang_rules.t ->
  Graph_from_AST.func_info list ->
  (string, Graph_from_AST.func_info list) Hashtbl.t

(* Bucket free funcs by file [Module_qn]; empty unless [`Per_file]. *)
val build_by_module :
  cfg:Index_lang_rules.t ->
  file_infos:Types.file_info list ->
  Graph_from_AST.func_info list ->
  (Names.Module_qn.t, Graph_from_AST.func_info list) Hashtbl.t
