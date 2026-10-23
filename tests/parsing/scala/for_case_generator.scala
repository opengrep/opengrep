object ForCaseGenerator {
  def parens(xs: List[(Int, Int)]) = for (case (a, b) <- xs) yield a + b

  def braces(xs: List[Option[Int]]) = for {
    case Some(a) <- xs
    case Some(b) <- xs
  } yield a + b

  def scala3(xs: List[(Int, Int)]) = for case (a, b) <- xs yield a + b
}
