(* Works by side effect on the generic AST by modifying its refs.
 * We pass the lang because some name resolution algorithm may be
 * specific to a language.
 *)
val resolve : Lang.t -> AST_generic.program -> unit
val members_in_scope_in_methods : Lang.t -> bool
val constructor_named_after_class : Lang.t -> bool
