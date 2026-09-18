package c

object Store {
  def sink(x: String): Unit = println(x)

  def handle(msg: String): Unit = {
    // ruleid: test-hiding-import
    sink(msg)
  }
}
