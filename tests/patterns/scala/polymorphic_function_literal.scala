object Foo:
  //ERROR:
  val f = [A] => (xs: List[A]) => sink(xs)

  //OK:
  val g = (xs: List[Int]) => log(xs)
