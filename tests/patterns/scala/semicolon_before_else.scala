object Foo {
  //ERROR:
  def f(x: Boolean) = if (x) sink(1); else log(2)

  //OK:
  def g(x: Boolean) = if (x) log(1) else sink(2)
}
