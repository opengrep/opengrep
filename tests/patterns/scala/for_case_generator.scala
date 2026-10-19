object Foo:
  def f(xs: List[Option[Int]]) =
    //ERROR:
    for case Some(x) <- xs do sink(x)

  def g(xs: List[Option[Int]]) =
    //OK:
    for case Some(x) <- xs do log(x)
