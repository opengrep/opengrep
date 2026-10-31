object M:
  // `given T` tests the type of the value, as `_: T` does, so each case is
  // reachable and its body is analysed.
  def f(x: Any): Unit =
    x match
      case given Ordering[Int] =>
        // ruleid: taint-scala-given-pattern
        sink(taint_source())
      case given Codec =>
        // ruleid: taint-scala-given-pattern
        sink(taint_source())
      case _ =>
        // ok: taint-scala-given-pattern
        sink("clean")

  // A given element keeps the positions of the other elements of a tuple.
  def tupleElements(): Unit =
    (ordering, taint_source()) match
      case (given Ordering[Int], y) =>
        // ruleid: taint-scala-given-pattern
        sink(y)
      case _ => ()
    (taint_source(), "clean") match
      case (given Ordering[Int], y) =>
        // ok: taint-scala-given-pattern
        sink(y)
      case _ => ()
