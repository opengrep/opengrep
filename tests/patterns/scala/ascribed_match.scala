object Foo:
  //ERROR:
  def f(x: Any) = x.toString: @unchecked match
    case s => sink(s)

  //OK:
  def g(x: Any) = x.toString: @unchecked
