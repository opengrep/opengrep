object Foo {
  def f(x: Option[String]) = x match {
    //ERROR:
    case Some(enum) => sink(enum)
    //OK:
    case None => log(1)
  }
}
