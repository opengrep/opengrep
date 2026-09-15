package app

object Store {
  def sink(x: String): Unit = println(x)

  def handle(msg: String): Unit = {
    // ruleid: test-qualified-companion-call
    sink(msg)
  }
}

class Store {
  def sink(x: String): Unit = println(x)

  def handle(msg: String): Unit = {
    // ok: test-qualified-companion-call
    sink(msg)
  }
}
