object Patterns {
  // a backquoted identifier with a type binds a variable
  def backquoted(): Unit = taint_source() match {
    case `type`: String =>
      // ruleid: taint-scala-typed-patterns
      sink(`type`)
    case _ => ()
  }

  // a literal with a type, as in Scala 3
  def literal(x: Any): Unit = x match {
    case 1: Int =>
      // ok: taint-scala-typed-patterns
      sink(x)
    case _ => ()
  }
}
