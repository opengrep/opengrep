object ForDo:
  def oneLine(xs: List[Int]) = for x <- xs do println(x)

  def parens(xs: List[Int]) = for (x <- xs) do println(x)

  def indented(xs: List[Int], ys: List[Int]) =
    for
      x <- xs
      if x > 0
      y <- ys
    do
      println(x)
      println(y)

  def bodyOnNextLine(xs: List[Int]) =
    for x <- xs do
      val y = x + 1
      println(y)
