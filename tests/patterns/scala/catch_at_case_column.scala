// `finally` at the column of the cases belongs to the `try`.
object Folds:
  def g(x: Int) =
    //ERROR:
    try x match
      case 1 => 2
      finally done()

  def h(x: Int) =
    //OK:
    try x match
      case 1 => 2
