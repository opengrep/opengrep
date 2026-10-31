object Foo:
  //ERROR:
  inline def f(inline x: Int) = inline if x > 0 then sink(x) else 0

  //OK:
  inline def g(inline x: Int) = inline if x > 0 then log(x) else 0
