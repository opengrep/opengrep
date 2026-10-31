object ForBodyStartingWithIf {
  def parens(xs: List[Int]) = {
    for (x <- xs) if (x > 0) println(x)
    done()
  }

  def braces(xs: List[Int]) = {
    for {
      x <- xs
    } if (x > 0) println(x)
    done()
  }

  def guards(xs: List[Int]) = {
    for (x <- xs; if x > 0) println(x)
    for {
      x <- xs
      if x > 0
    } println(x)
  }
}
