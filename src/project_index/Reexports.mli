(* [`Per_file]-only: [from .X import name] adds [name] to the importer's
   module. Pure: reads [project_funcs_by_module], returns the updated
   per-module func lists for the caller to apply. *)

val resolve_into_module_index :
  project_funcs_by_module:
    (Names.Module_qn.t, Graph_from_AST.func_info list) Hashtbl.t ->
  Types.file_info list ->
  (Names.Module_qn.t * Graph_from_AST.func_info list) list
