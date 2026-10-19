object Foo:
  //ERROR:
  transparent inline def f(x: Int): Int = sink(x)

  //OK:
  inline def g(x: Int): Int = sink(x)
