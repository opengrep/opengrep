object Foo:
  //ERROR:
  val a = xs.map: x =>
    x + 1

  //ERROR:
  val b = xs.map: (x) =>
    x + 1

  //ERROR:
  val d = SwissPairing.fields: F =>
    F + 1

  //ERROR:
  val e = coll.aggregateOne(): F =>
    F + 1

  // the colon stands for braces
  //ERROR:
  val f = xs.map { x =>
    x + 1
  }

  //OK:
  val c = { x: Int =>
    x + 1
  }

trait T { self: Any =>
  //OK:
  self + 1
}
