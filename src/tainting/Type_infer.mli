module G = AST_generic

(* The result is the bare class name of an iterable's element type ([[]T] and
   arrays). *)
val slice_element_of_ty : G.type_ -> G.type_ option
