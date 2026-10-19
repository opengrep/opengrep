class Foo:
  def f = this.match
    //ERROR:
    case _: Foo => sink(1)

  //OK:
  def g = this.toString
