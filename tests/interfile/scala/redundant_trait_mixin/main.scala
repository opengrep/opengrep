package app

object Main {
  def source(): String = sys.env("SECRET")

  def run(): Unit = {
    val t = source()
    new C().handle(t)
  }
}

trait T {
  def sink(x: String): Unit = println(x)

  def handle(msg: String): Unit = {
    // ok: redundant-trait-mixin
    sink(msg)
  }
}

class A extends T {
  override def handle(msg: String): Unit = {
    // ruleid: redundant-trait-mixin
    sink(msg)
  }
}

class C extends A with T
