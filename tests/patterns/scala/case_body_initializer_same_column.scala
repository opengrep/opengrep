object Foo {
  def f(x: Int) = x match {
    case _ =>
      //ERROR:
      val t =
      g(1)
      h(t)
  }

  def k(x: Int) = x match
    case _ =>
      //ERROR:
      val t =
      g(1)
      h(t)
}
