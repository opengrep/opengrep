package app

class Conv

object Conv {
  implicit def fromInt(n: Int): Conv = new Conv
}

object Helper {
  def sink(x: String): Unit = println(x)

  def handle(c: Conv, input: String): Unit = {
    // ruleid: implicit-view
    sink(input)
  }

  def drop(c: Conv, input: String): Unit = {
    // ok: implicit-view
    sink("")
  }
}
