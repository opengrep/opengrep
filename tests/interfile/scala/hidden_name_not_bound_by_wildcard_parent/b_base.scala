package b

class Base {
  def f(x: String): Unit = {
    // ruleid: hidden-name-not-bound-by-wildcard-parent
    sink(x)
  }
}
