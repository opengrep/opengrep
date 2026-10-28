object Folds:
  def f(x: Int) =
    try x match
      case 1 => 2
      case _ => 3
      catch case ex: Exception => 0

  def g(x: Int) =
    try x match
      case 1 => 2
      finally done()
