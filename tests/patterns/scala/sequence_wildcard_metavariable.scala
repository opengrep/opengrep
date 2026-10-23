object Foo:
  //ERROR:
  def f(xs: Seq[Int]) = xs match
    case Seq(first, rest*) => sink(rest)

  //OK:
  def g(xs: Seq[Int]) = xs match
    case Seq(first, rest*) => sink(first)
