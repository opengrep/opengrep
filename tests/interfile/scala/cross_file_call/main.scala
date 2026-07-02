package app

object Main {
  def source(): String = sys.env("SECRET")

  def run(): Unit = {
    val tainted = source()
    Impl.greet(tainted)
  }
}
