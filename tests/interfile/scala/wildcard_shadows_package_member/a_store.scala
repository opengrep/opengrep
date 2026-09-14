package a

object Store {
  def sink(x: String): Unit = println(x)

  def handle(msg: String): Unit = {
    // ruleid: test-wildcard-shadows-package-member
    sink(msg)
  }
}
