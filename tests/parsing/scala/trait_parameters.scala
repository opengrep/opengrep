trait Greeter(val name: String):
  def greet = s"hello $name"

trait Logging(level: Int) {
  def log(msg: String) = println(msg)
}

class A extends Greeter("a")
class B extends Base with Logging(1)
class C extends Base, Logging(2), Greeter("c")
