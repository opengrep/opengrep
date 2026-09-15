package app

class Store {
  def sink(x: String): Unit = println(x)

  def handle(msg: String): Unit = {
    // ruleid: test-instance-not-companion
    sink(msg)
  }
}

object Store {
  def sink(x: String): Unit = println(x)

  def handle(msg: String): Unit = {
    // ok: test-instance-not-companion
    sink(msg)
  }
}
