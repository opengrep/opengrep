// A colon at the end of the line, then the type of the given and = or with
object Givens:
  //ERROR:
  given conn:
    String = source()

  given fooOfInt:
    Foo[Int] with
    def foo(v: Int): Unit = ()

  // a template body
  given Foo[Long]:
    //OK:
    def foo(v: Long): Unit = ()

  // a statement of a template body, not the type of the given
  given Show[Int]:
    inline def show(x: Int): String = x.toString
    //OK:
    def other(v: Long): Unit = ()

  // an assignment in a template body, not the type of the given
  given Ord[Long]:
    count = 1
    //OK:
    def other(v: Long): Unit = ()
