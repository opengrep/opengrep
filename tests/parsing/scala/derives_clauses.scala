case class Point(x: Int, y: Int) derives CanEqual, Show

case class Body(x: Int) derives CanEqual:
  def f = x

enum Color derives CanEqual:
  case Red, Green

class Child extends Parent derives Show

object Identifier {
  val derives = 1
}
