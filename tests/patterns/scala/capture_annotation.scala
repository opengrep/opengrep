// A capture annotation is dropped from the type.
object Foo:
  //ERROR:
  def f(xs: IterableOnce[Int]^): Int = sink(xs)

  //ERROR:
  def g(xs: IterableOnce[Int]^{io}): Int = sink(xs)

  //OK:
  def h(xs: IterableOnce[Long]^): Int = sink(xs)
