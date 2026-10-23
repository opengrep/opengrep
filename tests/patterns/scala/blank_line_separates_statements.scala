object Foo {
  case class C(a: Int)

  //ERROR:
  (x, y).contramap2(f)

  //OK:
  (x, y).map(f)
}
