(* TS/JS post-pass: [class C { static foo = importedFn }] makes [C.foo(...)] resolve
   to [importedFn] via a synthetic method under [C]. No-op for non-TS/JS. *)

val add_class_body_aliases :
  lang:Lang.t ->
  project_funcs_by_name:(string, Graph_from_AST.func_info list) Hashtbl.t ->
  Types.file_info list ->
  Type_state.t -> Type_state.t
