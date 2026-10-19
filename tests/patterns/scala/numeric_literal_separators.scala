object Foo:
  //ERROR:
  val a = sink(99_999.999_99)

  //ERROR:
  val b = sink(0b0001_0000)

  //OK:
  val c = log(1.5)
