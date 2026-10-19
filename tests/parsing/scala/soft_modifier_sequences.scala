private[p] transparent trait A
transparent trait B
infix class Or[X, Y]

object SoftModifierSequences:
  transparent inline def f: Any = 1
  inline transparent def g: Any = 2
  infix def plus(y: Int) = y
  infix type +[X, Y] = Either[X, Y]

  def local(x: Int) =
    inline def twice(y: Int) = y * 2
    twice(x)

  extension (x: Int) infix def times(y: Int) = x * y

class Identifiers(open: String) {
  def bar(n: Int) = n match {
    case 0 => open
    case _ => ""
  }
}
