trait A
trait B
class C extends A, B
class D extends A, B:
  def f = 1
object E extends A, B
class F extends A with B
object G:
  val xs = List(new A {}, new B {})
  def g(a: A, b: Int) = a
  val y = g(new A {}, 1)
