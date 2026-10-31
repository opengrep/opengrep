class Synthesizer(typer: Typer)(using @constructorOnly c: Context):
  def f = c

object UsingParameterAnnotations:
  extension (coll: Coll)(using @annotation.nowarn ex: Executor)
    def secondary = coll
  def g(using @unused ctx: Context, other: Int) = other
