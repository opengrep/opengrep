object Foo:
  def f(ctxs: List[Any]) =
    ctxs.foreach {
      //ERROR:
      case ctx @ given Context => sink(ctx)
      //OK:
      case other => log(other)
    }
