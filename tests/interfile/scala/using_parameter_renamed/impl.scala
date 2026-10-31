package app

trait B { def f(a: String)(using c: Ctx): Unit }

class K2 extends B {
  def f(a: String)(using q: Ctx): Unit = ()
}

class K1 extends B {
  def f(a: String)(using ctx: Ctx): Unit =
    // ruleid: test-using-parameter-renamed
    sink(a)
}
