package app

trait Base {
  def sink(x: String): Unit = println(x)

  def handle(msg: String): Unit = {
    // ruleid: test-companion-inherits-trait
    sink(msg)
  }
}

object Store extends Base

class Store {
  def sink(x: String): Unit = println(x)

  def handle(msg: String): Unit = {
    // ok: test-companion-inherits-trait
    sink(msg)
  }
}
