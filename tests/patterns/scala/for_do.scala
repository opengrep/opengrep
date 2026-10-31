object Foo:
  def f(xs: List[Int]) =
    //ERROR:
    for x <- xs do sink(x)

  def g(xs: List[Int]) =
    //OK:
    for x <- xs do log(x)
