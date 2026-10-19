object ClosingTokenClosesIndentRegion {
  def paren(xs: List[Int]) =
    xs.foreach(x =>
      if (x > 0)
        println(x))

  def matchInParens(p: Option[Int]) =
    p.isEmpty && (p match
      case None => true
      case Some(x) => x > 0)

  def comma(xs: List[Int]) =
    xs.foldLeft(0)((acc, x) =>
      if (x > 0)
        acc + x
      else acc, 1)

  def brace(xs: List[Int]) = {
    xs.foreach { x =>
      if (x > 0)
        println(x) }
  }
}
