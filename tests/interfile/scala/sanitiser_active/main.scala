package app

object Main {
  def source(): String = sys.env("SECRET")

  def run(): Unit = {
    val x = source()
    val y = Mid.process(x)
    Impl.emit(y)
  }
}
