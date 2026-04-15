(* Works by side effect on the generic AST by modifying its refs.
 * We pass the lang because some name resolution algorithm may be
 * specific to a language.
 *)
val resolve : Lang.t -> AST_generic.program -> unit

(* True for languages where a plain assignment `x = e` may be the first and
 * only declaration of `x` (Python, Ruby, PHP, JS/TS). *)
val assign_implicitly_declares : Lang.t -> bool

val pro_hook_normalize_ast_generic_type :
  (Lang.t -> AST_generic.type_ -> AST_generic.type_) option ref
