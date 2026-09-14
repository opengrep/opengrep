package a

object Store {
  def sink(x: String): Unit = println(x)

  def handle(msg: String): Unit = {
    // ok: test-hiding-import
    sink(msg)
  }
}
