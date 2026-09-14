package c

import a.{Store => S}

object Main {
  def source(): String = sys.env("SECRET")

  def run(): Unit = {
    val t = source()
    S.handle(t)
  }
}
