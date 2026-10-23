object Foo:
  //ERROR:
  transparent inline def f(x: Int): Int = sink(x)

  //OK:
  def g(x: Int): Int = log(x)
