object Foo:
  //ERROR:
  def f[T: {Ordering as ord, Numeric}](x: T) = sink(ord)

  //OK:
  def g[T: Ordering](x: T) = log(x)
