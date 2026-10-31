// `rest: _*` binds the rest of the sequence, as `rest*` does.
object Foo:
  //ERROR:
  def f(xs: Seq[Int]) = xs match
    case Seq(first, rest: _*) => sink(rest)

  //OK:
  def h(xs: Seq[Int]) = xs match
    case Seq(first, rest: Int) => sink(rest)
