object Foo {
  def f: Eq[Int] =
    //ERROR:
    (a, b) =>
      foo(a, b)

  val h =
    //ERROR:
    (a, b) => foo(a, b)

  val k =
    //OK:
    (a, b) => bar(a, b)
}
