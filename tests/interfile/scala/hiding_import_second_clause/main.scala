package c

import p.{Store => _, _}
import p._

object Main {
  def source(): String = sys.env("SECRET")

  def run(): Unit = {
    val t = source()
    Store.handle(t)
  }
}
