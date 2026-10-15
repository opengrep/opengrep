val function_definition :
  Lang.t ->
  AST_generic.function_definition ->
  IL.function_definition

val lambda_definition :
  Lang.t ->
  enclosing_captures:AST_generic.captures list ->
  AST_generic.captures option ->
  AST_generic.function_definition ->
  IL.function_definition
(** A lambda lowered on its own, with its captures and those of the lambdas
    enclosing it, outermost first. *)

val stmt : Lang.t -> AST_generic.stmt -> IL.stmt list
val expr : Lang.t -> AST_generic.expr -> IL.exp
val lval : Lang.t -> AST_generic.expr -> IL.lval
val name_of_entity : AST_generic.entity -> IL.name option
val var_of_name : AST_generic.name -> IL.name
val var_of_id_info : AST_generic.ident -> AST_generic.id_info -> IL.name
