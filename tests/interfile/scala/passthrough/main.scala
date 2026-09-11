package app

object Main {
  def source(): String = sys.env("SECRET")

  def run(): Unit = {
    val t = source()
    Mid.relay(t)
  }
}
