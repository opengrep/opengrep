// `outer: c =>` at the start of a template body is a self type, not a call
// of `outer` with a colon argument.
trait A {
  //OK:
  outer: c =>
    def f = run(1)
}

trait B {
  //ERROR:
  def g = outer(1)
}
