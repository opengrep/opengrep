package pkg

object Store {
  def sink(x: String): Unit = println(x)

  def handle(msg: String): Unit = {
    // ok: qualified-class-in-type-position
    sink(msg)
  }
}

class Store {
  def handle(msg: String): Unit = {
    println(msg)
  }
}
