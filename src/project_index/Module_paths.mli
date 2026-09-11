val module_qn_of_file :
  cfg:Index_lang_rules.t ->
  go_modules:Go_modules.t ->
  project_root:Fpath.t ->
  ast:AST_generic.program option ->
  Fpath.t ->
  Names.Module_qn.t

val module_name_string :
  cfg:Index_lang_rules.t ->
  current_module_path:Names.Module_qn.t ->
  is_init_file:bool ->
  AST_generic.module_name ->
  Names.Module_qn.t

val enclosing_package :
  cfg:Index_lang_rules.t ->
  file:Fpath.t ->
  Names.Module_qn.t ->
  Names.Module_qn.t
