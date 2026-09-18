package a

object Main {
  def source(): String = sys.env("SECRET")

  def run(): Unit = {
    val t = source()
    Store.handle(t)
  }
}
