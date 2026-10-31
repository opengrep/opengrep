package app

object Impl {
  def f(implicit c: String): Unit =
    // ruleid: test-using-argument-implicit
    sink(c)
  def f(a: Int, b: Int): Unit = ()
}
