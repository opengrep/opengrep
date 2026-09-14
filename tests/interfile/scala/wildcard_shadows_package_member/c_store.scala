package c

object Store {
  def sink(x: String): Unit = println(x)

  def handle(msg: String): Unit = {
    // ok: test-wildcard-shadows-package-member
    sink(msg)
  }
}
