object Foo:
  //ERROR:
  def f(xs: IterableOnce[Int]^): Int = sink(xs)

  //OK:
  def g(xs: IterableOnce[Int]): Int = log(xs)
