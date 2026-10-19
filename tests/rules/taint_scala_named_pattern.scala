object M:
  // The name in `name = n` selects an element and binds nothing, so the
  // outer `name` is still the tainted variable.
  def outerVariable(p: Any): Unit =
    val name = taint_source()
    p match
      case Person(name = n) =>
        // ruleid: taint-scala-named-pattern
        sink(name)
      case _ => ()

  // Each named element is bound from the element of the same name.
  def perElement(): Unit =
    val t = (name = taint_source(), age = 1)
    t match
      case (name = n, age = a) =>
        // ruleid: taint-scala-named-pattern
        sink(n)
        // ok: taint-scala-named-pattern
        sink(a)
