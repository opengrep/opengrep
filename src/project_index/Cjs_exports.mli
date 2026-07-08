(* TS/JS: [module.exports = arrow] / [= NamedFn] defines the file's default-export
   function value, keyed by file path so cross-file [require('./file')] callbacks
   resolve. Anonymous arrows here aren't in [all_funcs] otherwise. No-op for non-TS/JS. *)

val build_default_export_fn :
  lang:Lang.t ->
  Types.file_info list ->
  (string, Graph_from_AST.func_info) Hashtbl.t
