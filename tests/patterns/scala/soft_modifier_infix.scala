object Foo:
  case class Money(n: Int):
    //ERROR:
    infix def plus(m: Money): Int = n + m.n

  //OK:
  def minus(x: Int, y: Int): Int = x - y
