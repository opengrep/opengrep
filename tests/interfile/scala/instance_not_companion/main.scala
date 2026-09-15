package app

object Main {
  def source(): String = sys.env("SECRET")

  def run(): Unit = {
    val t = source()
    val s = new Store()
    s.handle(t)
  }
}
