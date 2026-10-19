object Foo {
  //ERROR:
  val a = sink(raw"""\d\""")

  //OK:
  val b = log(raw"""\d""")
}
