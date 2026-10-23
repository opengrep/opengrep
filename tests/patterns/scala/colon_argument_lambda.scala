object Foo:
  //ERROR:
  val a = xs.map: x =>
    x + 1

  //ERROR:
  val b = xs.map: (x) =>
    x + 1

  //OK:
  val c = { x: Int =>
    x + 1
  }

trait T { self: Any =>
  //OK:
  self + 1
}
