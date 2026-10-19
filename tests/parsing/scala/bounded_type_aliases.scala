object BoundedTypeAliases:
  opaque type Id <: Int = Int
  opaque type Streakable <: Boolean = Boolean
  opaque type Range >: Int <: AnyVal = Int

  type Count[N, T] <: Tuple = (N, T) match
    case (0, _) => EmptyTuple
    case _ => T *: EmptyTuple

  type Elem[X] = X match
    case String => Char
    case Array[t] => t

  type Second[T <: Tuple] <: Any = T match { case (a, b) => b }
