object Colours:
  // `Red @ _` binds `Red` to the value it matches.
  def bound(): Unit =
    taint_source() match
      case Red @ Some(_) =>
        // ruleid: taint-scala-upper-case-binder
        sink(Red)
      case _ => ()

  def clean(): Unit =
    "clean" match
      case Blue @ Some(_) =>
        // ok: taint-scala-upper-case-binder
        sink(Blue)
      case _ => ()
