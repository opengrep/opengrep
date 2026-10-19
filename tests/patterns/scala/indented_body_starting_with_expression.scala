object Foo:
  //ERROR:
  def f(x: Int) =
    if x > 0 then println(x)
    sink(x)

  //OK:
  def g(x: Int) =
    if x > 0 then println(x)

  sink(1)
