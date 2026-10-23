object Foo {
  val x =
    //ERROR:
    a +
  b

  f(e =
    //ERROR:
    a +
  { b })

  val y =
    //OK:
    a
  b
}
