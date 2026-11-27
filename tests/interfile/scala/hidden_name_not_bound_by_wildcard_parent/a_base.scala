package a

class Base {
  def f(x: String): Unit = {
    // ok: hidden-name-not-bound-by-wildcard-parent
    sink(x)
  }
}
