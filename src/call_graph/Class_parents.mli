type side =
  | Instance_side
  | Class_side

type t = {
  written : AST_generic.type_;
  placement : Linearisation.placement;
  side : side;
}

val of_definition : Lang.t -> AST_generic.definition_kind -> t list
val definition_body : AST_generic.definition_kind -> AST_generic.stmt list

type singleton_exposure =
  | No_singleton_exposure
  | Every_method_is_a_singleton
  | Named_singleton_methods of string list

val singleton_exposure :
  Lang.t -> AST_generic.definition_kind -> singleton_exposure

val exposes : singleton_exposure -> string -> bool

val reopens :
  Lang.t -> AST_generic.entity -> AST_generic.definition_kind -> bool
