object Foo:
  //ERROR:
  val p = (name = sink(1), age = 2)

  //OK:
  val q = (sink(1), 2)
