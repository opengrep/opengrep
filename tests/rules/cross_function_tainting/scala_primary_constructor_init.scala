// The class header is the primary constructor: a construction passes its
// arguments to the header's parameters, and the statements of the class body
// run with them in source order.
class Foo(x: String) {
  // ruleid: scala_primary_constructor_init
  sink(x)
}

class Bar(x: String) {
  val y = x
  // ruleid: scala_primary_constructor_init
  sink(y)
}

class Baz(x: String) {
  // ok: scala_primary_constructor_init
  sink(x)
}

object Main {
  def direct(): Unit = {
    val foo = new Foo(source())
    val bar = new Bar(source())
    val baz = new Baz("constant")
  }
}
