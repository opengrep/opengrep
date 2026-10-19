// A context function is a lambda, so a lambda pattern matches it.
object Foo {
  //ERROR:
  def f = Open { ctx ?=> sink(ctx) }

  //ERROR:
  def g = Open { x => sink(x) }

  //OK:
  def h = Open { x => log(x) }
}
