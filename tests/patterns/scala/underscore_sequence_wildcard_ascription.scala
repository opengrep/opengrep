// `_: _*` is `_*`.
object A {
  //ERROR:
  def f(x: Any) = x match {
    case Seq(_: _*) => 1
  }
}
