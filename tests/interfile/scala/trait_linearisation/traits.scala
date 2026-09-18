package app

class Base {
  def handle(msg: String): Unit = ()
}

trait M1 extends Base {
  def sink(x: String): Unit = println(x)

  override def handle(msg: String): Unit = {
    // ok: test-trait-linearisation
    sink(msg)
  }
}

trait M2 extends Base {
  def sink(x: String): Unit = println(x)

  override def handle(msg: String): Unit = {
    // ruleid: test-trait-linearisation
    sink(msg)
  }
}

class Sub extends Base with M1 with M2
