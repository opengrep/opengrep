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
