package other

object Main {
  def source(): String = sys.env("SECRET")

  def run(s: pkg.Store): Unit = {
    val t = source()
    s.handle(t)
  }

  def build(): Unit = {
    val t = source()
    val made = new pkg.Store()
    made.handle(t)
  }
}
