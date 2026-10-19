object Foo:
  def f(xs: List[Int]) =
    for
      n = xs.size
      x <- xs
    //ERROR:
    yield sink(n)

  def g(xs: List[Int]) =
    for
      x <- xs
    //OK:
    yield log(x)
