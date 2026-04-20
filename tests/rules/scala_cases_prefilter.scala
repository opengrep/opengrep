object Example {
  val xs = List(1, 2, 3)
  // ruleid: find-scala-cases
  val ys = xs.map { case 1 => "one"; case n => "other" }
}
