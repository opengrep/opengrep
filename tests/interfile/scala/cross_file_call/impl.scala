package app

object Impl {
  def sink(x: String): Unit = println(x)

  def greet(msg: String): Unit = {
    // ruleid: test-cross-file-call
    sink(msg)
  }
}
