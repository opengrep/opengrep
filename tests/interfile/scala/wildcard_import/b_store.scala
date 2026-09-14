package b

object Store {
  def sink(x: String): Unit = println(x)

  def handle(msg: String): Unit = {
    // ok: test-wildcard-import
    sink(msg)
  }
}
