package app

object Impl {
  def sink(x: String): Unit = println(x)

  def leak(x: String): Unit = {
    // ruleid: test-passthrough
    sink(x)
  }
}
