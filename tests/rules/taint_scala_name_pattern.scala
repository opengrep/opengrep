object M:
  // The name in `name = n` selects an element and binds nothing, so the
  // outer `name` is still the tainted variable.
  def outerVariable(p: Any): Unit =
    val name = taint_source()
    p match
      case Person(name = n) =>
        // ruleid: taint-scala-name-pattern
        sink(name)
      case _ => ()

  // Each `name = pattern` binds the element with that name.
  def perElement(): Unit =
    val t = (name = taint_source(), age = 1)
    t match
      case (name = n, age = a) =>
        // ruleid: taint-scala-name-pattern
        sink(n)
        // ok: taint-scala-name-pattern
        sink(a)

  // An element of a named tuple is a field, not a variable: the outer `name`
  // is still the tainted one.
  def elementIsNotAVariable(): Unit =
    val name = taint_source()
    val t = (name = "clean", age = 1)
    // ruleid: taint-scala-name-pattern
    sink(name)

  // `kind` and a backquoted `kind` are the same element.
  def backquotedKey(): Unit =
    val t = (kind = 1, age = taint_source())
    t match
      case (`kind` = k, age = a) =>
        // ok: taint-scala-name-pattern
        sink(k)
        // ruleid: taint-scala-name-pattern
        sink(a)
