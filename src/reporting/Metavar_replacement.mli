type replacement_ctx

(* for abstract_content and subpatterns matching-explanations
 * TODO: should not use! the result may miss some commas
 *)
val metavar_string_of_any : AST_generic.any -> string

val of_bindings : Metavariable.bindings -> replacement_ctx
val of_out : Semgrep_output_v1_t.metavars -> replacement_ctx
val interpolate_metavars : string -> replacement_ctx -> string
