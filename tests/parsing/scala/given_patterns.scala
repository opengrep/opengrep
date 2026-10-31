object GivenPatterns:
  def binding(x: Any) = x match
    case given Int => 1
    case ctx @ given String => ctx.length

  def forBinding(me: Option[Me]) =
    for
      given Option[Me] <- loadMe
      given Perf = me.fold(Perf.default)(_.perf)
      blocking <- fetchBlocking
    yield blocking

object GivenAsVariable {
  def f(x: Any) = x match {
    case given :: rest => rest
    case (given, other) => other
    case given @ Some(_) => 1
    case given => 2
  }
}
