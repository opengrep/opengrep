package constructor_taint

object Main {
  def source(): String = {
    sys.env.getOrElse("SECRET", "")
  }

  def main(args: Array[String]): Unit = {
    val tainted = source()
    val s = new Store(tainted)
    s.send()
  }
}
