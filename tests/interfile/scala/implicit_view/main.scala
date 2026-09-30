package app

object Main {
  def source(): String = sys.env("SECRET")

  def run(): Unit = {
    val tainted = source()
    Helper.handle(1, tainted)
    Helper.drop(1, tainted)
  }
}
