object Foo {
  //ERROR:
  def f = Open { ctx ?=> sink(ctx) }

  //OK:
  def g = Open { x => sink(x) }
}
