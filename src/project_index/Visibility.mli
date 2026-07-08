(* Per-file in-scope leaf names for bare-name call resolution, so builtins
   ([all], [isinstance]) don't match same-named project funcs. *)

val build_dir_index :
  cfg:Index_lang_rules.t ->
  Types.file_info list ->
  (string, (string, unit) Hashtbl.t) Hashtbl.t

val for_file :
  cfg:Index_lang_rules.t ->
  dir_visible_names:(string, (string, unit) Hashtbl.t) Hashtbl.t ->
  project_funcs_by_module:(Names.Module_qn.t, Graph_from_AST.func_info list) Hashtbl.t ->
  Types.file_info ->
  (string, unit) Hashtbl.t
