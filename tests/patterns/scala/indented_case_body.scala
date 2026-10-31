object Foo {
  def f(x: Int) =
    //ERROR:
    x match {
      case 1 =>
        foo(1)
        bar(2)
    }

  def g(x: Int) =
    //OK:
    x match {
      case 1 =>
        foo(1)
        baz(2)
    }
}
