object ForAliasFirst:
  def indented(xs: List[Int]) =
    for
      n = xs.size
      x <- xs
    yield x + n

  def braces(xs: List[Int]) = for { n = xs.size; x <- xs } yield x * n
