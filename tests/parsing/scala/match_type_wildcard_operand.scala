object Tuples:
  type Head[X <: Tuple] = X match
    case _ *: EmptyTuple => Nothing
    case h *: _ => h

  type IsEmpty[X] = X match {
    case _ => Boolean
  }
