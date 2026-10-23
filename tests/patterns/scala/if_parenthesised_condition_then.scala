object Foo:
  def f(a: Boolean, b: Boolean) =
    //ERROR:
    if (a) && (b) then sink(a)

  def g(a: Boolean, b: Boolean) =
    //OK:
    if (a) sink(b)
