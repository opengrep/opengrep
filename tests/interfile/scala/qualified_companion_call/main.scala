package other

object Main {
  def source(): String = sys.env("SECRET")

  def run(): Unit = {
    val t = source()
    app.Store.handle(t)
  }
}
