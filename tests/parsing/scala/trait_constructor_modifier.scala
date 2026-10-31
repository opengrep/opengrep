// A trait, which takes parameters, can have a constructor modifier.
trait T private[a] (x: Int) {
  def f = 1
}
trait U private {
  def g = 2
}
