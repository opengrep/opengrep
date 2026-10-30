// A polymorphic function literal can be a colon argument.
object A:
  def h =
    handle(tag): [C] =>
      (a, cont) =>
        f(a).andThen(cont(()))
  def k = 1
