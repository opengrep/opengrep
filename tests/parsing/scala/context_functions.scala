object ContextFunctions:
  type Handler = String ?=> Int
  type Handler2 = (String, Int) ?=> Int

  def braces = Auth { ctx ?=> me ?=>
    ctx.length + me
  }

  def colon = Open: ctx ?=>
    ctx.length

  def parens = run(s ?=> s.length)

  def typed = run((s: String) ?=> s.length)

  def wildcard = AuthBody: _ ?=>
    1

  val f: String ?=> Int = s ?=> s.length
