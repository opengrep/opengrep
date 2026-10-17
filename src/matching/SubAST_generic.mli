val subexprs_of_expr :
  ?symbolic_propagation:bool -> AST_generic.expr -> AST_generic.expr list

val subexprs_of_expr_implicit :
  ?symbolic_propagation:bool -> AST_generic.expr -> AST_generic.expr list

val subexprs_of_stmt_kind : AST_generic.stmt_kind -> AST_generic.expr list

(* The outermost expressions a pattern holds, a literal included. *)
val subexprs_of_pattern : AST_generic.pattern -> AST_generic.expr list

(* The expressions of a case of a switch: its value, or those of its
 * pattern. *)
val subexprs_of_case : AST_generic.case -> AST_generic.expr list

(* The expressions of the cases of a branch of a switch. *)
val subexprs_of_cases : AST_generic.case list -> AST_generic.expr list

val flatten_substmts_of_stmts :
  AST_generic.stmt list -> (AST_generic.stmt list * AST_generic.stmt) option
