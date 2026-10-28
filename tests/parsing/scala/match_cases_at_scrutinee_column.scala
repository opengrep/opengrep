// The cases of a match can be at the column of the scrutinee, also in
// parentheses.
object A:
  def f(x: Option[Int]) =
    Expr(
      x match
      case None => 1
      case Some(n) => n
    )
