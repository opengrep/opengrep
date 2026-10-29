package app

object Main {
  def run(x: B): Unit = {
    given Ctx = mk()
    x.f(source())
  }
}
