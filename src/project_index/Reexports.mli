(* [bound -> target] for every name an [__init__]-style package file
   re-exports; empty unless [cfg.has_reexports]. *)
val build_reexport_map :
  cfg:Index_lang_rules.t ->
  Types.file_info list ->
  (Names.Module_qn.t, Names.Module_qn.t) Hashtbl.t

(* [`Per_file]-only: [from .X import name] adds [name] to the importer's
   module. Pure: reads [project_funcs_by_module], returns the updated
   per-module func lists for the caller to apply. *)

val resolve_into_module_index :
  project_funcs_by_module:
    (Names.Module_qn.t, Graph_from_AST.func_info list) Hashtbl.t ->
  dunder_all:(string, unit) Hashtbl.t Common.SMap.t ->
  Types.file_info list ->
  (Names.Module_qn.t * Graph_from_AST.func_info list) list

val build_dunder_all :
  file_infos:Types.file_info list -> (string, unit) Hashtbl.t Common.SMap.t

val star_exported :
  dunder_all:(string, unit) Hashtbl.t Common.SMap.t ->
  Names.Module_qn.t -> string -> bool
