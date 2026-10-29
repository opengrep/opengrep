package app

trait B { def f(a: String)(using Ctx): Unit }

class K2 extends B {
  def f(a: String)(using Ctx): Unit = ()
}

class K extends B {
  def f(context: String)(using Ctx): Unit =
    // ruleid: test-using-parameter-named-context
    sink(context)
}
