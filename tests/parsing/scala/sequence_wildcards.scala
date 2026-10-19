case class *(a: Int, b: Int)

object SequenceWildcards:
  def scala3(xs: Seq[Int]) = xs match
    case Seq(a, rest*) => rest
    case Seq(a, b, c *) => c

  def scala2(xs: Seq[Int]) = xs match {
    case Seq(a, rest @ _*) => rest
    case Seq(_*) => Nil
  }

  def trailingComma(xs: List[Int]) = xs match {
    case List(1, 2, _ @ _*,
    ) => 1
  }

  val x * y = new *(6, 7)
