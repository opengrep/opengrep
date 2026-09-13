type specifier_resolution =
  | Specifier_is_module_name
  | Specifier_names_file of module_files

and module_files

val module_of_specifier :
  module_files -> current_file:Fpath.t -> string -> Names.Module_qn.t option

val specifier_resolution_of_files :
  cfg:Index_lang_rules.t ->
  project_root:Fpath.t ->
  paths:(string * string list) list ->
  Fpath.t list ->
  specifier_resolution

val module_qn_of_file :
  cfg:Index_lang_rules.t ->
  go_modules:Go_modules.t ->
  rust_crates:Rust_crates.t ->
  project_root:Fpath.t ->
  ast:AST_generic.program option ->
  Fpath.t ->
  Names.Module_qn.t

val relative_module_qn :
  current:Names.Module_qn.t ->
  Index_lang_rules.relative_module ->
  Names.Module_qn.t

val module_name_string :
  cfg:Index_lang_rules.t ->
  resolution:specifier_resolution ->
  current_file:Fpath.t ->
  current_module_path:Names.Module_qn.t ->
  own_module_names:unit Common.SMap.t ->
  is_init_file:bool ->
  AST_generic.module_name ->
  Names.Module_qn.t option

val enclosing_package :
  cfg:Index_lang_rules.t ->
  file:Fpath.t ->
  Names.Module_qn.t ->
  Names.Module_qn.t
