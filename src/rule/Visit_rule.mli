val visit_formula : (Rule.formula -> unit) -> Rule.formula -> unit

val visit_xpatterns :
  (Xpattern.t -> inside:bool -> 'a -> 'a) -> Rule.formula -> 'a -> 'a

val xpatterns_of_rule : Rule.t -> Xpattern.t list
