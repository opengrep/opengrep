type exports
type project_scope

type file_bindings = {
  fb_scope : Func_lookup.scope_entry list Common.SMap.t;
  fb_module_aliases : (string, Names.Module_qn.t) Hashtbl.t;
  fb_own_modules : Names.Module_qn.t list;
}

val no_project_scope : project_scope

val build_project_scope :
  definitions_by_qn:Types.definition Common.SMap.t ->
  value_alias_index:(string * string, AST_generic.expr) Hashtbl.t ->
  classes_by_file:Types.entry list Common.SMap.t ->
  class_parent_paths:
    (Function_id.t * IL.name option list) list Common.SMap.t ->
  file_funcs_index:(string, Func_info.t list) Hashtbl.t ->
  file_infos:Types.file_info list ->
  project_scope

val exports_of : project_scope -> exports

val class_aliases_of :
  project_scope -> (Names.Class_qn.t * string * Func_info.t list) list

val exported_names : exports -> unit Common.SMap.t Common.SMap.t

val build :
  scope:project_scope ->
  classes_by_file:Types.entry list Common.SMap.t ->
  class_parent_paths:
    (Function_id.t * IL.name option list) list Common.SMap.t ->
  file_funcs_index:(string, Func_info.t list) Hashtbl.t ->
  Types.file_info ->
  file_bindings
