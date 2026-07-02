package app

object Impl {
  def sink(x: String): Unit = println(x)

  def emit(data: String): Unit = {
    // ok: test-sanitiser
    sink(data)
  }
}
