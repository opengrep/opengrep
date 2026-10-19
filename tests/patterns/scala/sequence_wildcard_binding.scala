object Foo:
  //ERROR:
  def f(xs: Seq[Int]) = xs match
    case Seq(first, rest*) => sink(rest)

  //ERROR:
  def g(xs: Seq[Int]) = xs match
    case Seq(first, rest @ _*) => sink(rest)

  //OK:
  def h(xs: Seq[Int]) = xs match
    case Seq(first, rest) => sink(rest)
