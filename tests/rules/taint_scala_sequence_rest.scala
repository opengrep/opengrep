object M:
  // `rest*` and `rest @ _*` bind the elements after the fixed ones, a view of
  // the scrutinee from the position of the wildcard onwards.
  def scala3Form(): Unit =
    val xs = Seq("a", "b", taint_source())
    xs match
      case Seq(first, rest*) =>
        // ruleid: taint-scala-sequence-rest
        sink(rest)
      case _ => ()

  def scala2Form(): Unit =
    val xs = Seq("a", "b", taint_source())
    xs match
      case Seq(first, rest @ _*) =>
        // ruleid: taint-scala-sequence-rest
        sink(rest)
      case _ => ()

  // `rest*` binds a new `rest` from the scrutinee, which hides the tainted
  // one outside.
  def restOfACleanSequence(): Unit =
    val rest = taint_source()
    val xs = Seq("a", "b", "c")
    xs match
      case Seq(first, rest*) =>
        // ok: taint-scala-sequence-rest
        sink(rest)
      case _ => ()
