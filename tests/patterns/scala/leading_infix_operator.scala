object Foo:
  def f(a: Int) =
    //ERROR:
    a
    - 1

  def g(a: Int) =
    //OK:
    a
    -1

  def h(a: Int) =
    //OK:
    a

    - 1
