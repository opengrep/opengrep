object IndentedBodies:
  def startsWithExpression(x: Int): Int =
    if x > 0 then
      1
    else
      2
    end if
  end startsWithExpression

  def selectionAfterRegion(xs: List[Int]) =
    for
      x <- xs
    yield x + 1
  .sum

  def caseAtMatchWidth(x: Any) = x match {
    case m: Int =>
      m match
      case 1 => "one"
      case _ => "other"
    case _ => "none"
  }

class SelfTypeArrow { self =>
    if (enabled) count()
  private val total = 0
}

object SemicolonBeforeDedent {
  for (
    a <- xs;
    b <-
      Seq(1, 2);
    c <- ys
  ) {
    f(a, b, c)
  }
}

class MemberAfterIndentedBlock {
  val entry =
    new Entry(1)

    override def toString: String = "entry"
}
