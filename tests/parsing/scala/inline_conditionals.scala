object InlineConditionals:
  inline def f(inline x: Int) = inline if x > 0 then 1 else 2

  inline def g(x: Any) = inline x match
    case _: Int => 1
    case _ => 2

  inline def h(inline p: Int => Boolean): Boolean = inline this match
    case _ => true

  def identifier(inline: Int) = inline + 1
