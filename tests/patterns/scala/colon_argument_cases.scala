object Foo:
  def g(xs: List[Option[Int]]) =
    xs.map:
      //ERROR:
      case None => sink(1)
      case Some(x) => x
