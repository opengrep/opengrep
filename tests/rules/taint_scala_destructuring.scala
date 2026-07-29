object M {
  // Destructuring declaration. The entity name is an EPattern holding a
  // PatTuple; before it was lowered through pattern_assign_statements,
  // `a` and `b` were never assigned in the IL.
  def declForm(): Unit = {
    val (a, b) = taint_source()

    // ruleid: taint-scala-destructuring
    sink(a)

    // ruleid: taint-scala-destructuring
    sink(b)
  }

  // Each slot lowers as `pat_i = tmp[i]`, so a clean slot stays clean.
  def perSlotPrecision(): Unit = {
    val (e, f) = (taint_source(), 1)

    // ruleid: taint-scala-destructuring
    sink(e)

    // ok: taint-scala-destructuring
    sink(f)
  }

  // A constructor pattern is rewritten into a tuple by `pattern`, so it
  // binds its arguments too.
  def constructorPattern(): Unit = {
    val Some(g) = taint_source()

    // ruleid: taint-scala-destructuring
    sink(g)
  }

  // A single (non-destructuring) binding keeps its existing lowering.
  def singleBinding(): Unit = {
    val c = taint_source()

    // ruleid: taint-scala-destructuring
    sink(c)
  }

  def clean(): Unit = {
    val d = 1

    // ok: taint-scala-destructuring
    sink(d)
  }
}
