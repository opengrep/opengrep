import b.Base

class Client extends Base

object App {
  def source(): String = sys.env("X")
  def main(args: Array[String]): Unit = {
    val t = source()
    new Client().handle(t)
  }
}
