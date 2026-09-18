package app

object Store {
  def sink(x: String): Unit = println(x)

  def handle(msg: String): Unit = {
    // ruleid: test-companion-object-first
    sink(msg)
  }
}

class Store {
  def sink(x: String): Unit = println(x)

  def handle(msg: String): Unit = {
    // ok: test-companion-object-first
    sink(msg)
  }
}
