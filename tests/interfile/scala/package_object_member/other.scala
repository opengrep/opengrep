package b

object util {
  def sink(x: String): Unit = println(x)

  def handle(msg: String): Unit = {
    // ok: test-package-object-member
    sink(msg)
  }
}
