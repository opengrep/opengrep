package app

class Store {
  def sink(x: String): Unit = println(x)

  def handle(msg: String): Unit = {
    // ok: test-companion-object-member
    sink(msg)
  }
}

object Store {
  def sink(x: String): Unit = println(x)

  def handle(msg: String): Unit = {
    // ruleid: test-companion-object-member
    sink(msg)
  }
}
