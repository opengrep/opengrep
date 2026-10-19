object Foo:
  //ERROR:
  val p = (name = sink(1), age = 2)

  //ERROR:
  val q = (name = 1, age = sink(2), city = "x")

  //OK:
  val r = (name = 2, city = sink(1))
