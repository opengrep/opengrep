// A named tuple is a record, whose elements match in any order.
object Foo:
  //ERROR:
  val p = (name = sink(1), age = 2)

  //OK:
  val q = (sink(1), 2)
