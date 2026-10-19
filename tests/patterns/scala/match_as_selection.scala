object Foo:
  //ERROR:
  def f(x: Option[Int]) = x.match
    case Some(y) => sink(y)
    case None => 0

  //OK:
  def g(x: Option[Int]) = x.get
