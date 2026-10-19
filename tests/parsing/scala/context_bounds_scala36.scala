object ContextBoundsScala36:
  def named[T: Ordering as ord](x: T) = ord
  def several[T: {Ordering, Numeric}](x: T) = x
  def severalNamed[T: {Ordering as ord, Numeric as num}](x: T) = num
  def lambda[A, B: [X] =>> Combinator[X] { type Context = A }](x: B) = x
  def classic[T: Ordering: Numeric](x: T) = x
