object MatchSelectionAndChains:
  def selection(x: Int) = x.match { case 1 => "a"; case _ => "b" }.length

  def indented(x: Option[Int]) = x.match
    case Some(y) => y
    case None => 0

  def path(a: A) = a.b.match
    case 1 => true
    case _ => false

  def chained(x: Int) = x match
    case 1 => 2
    case _ => 3
  match
    case 2 => "two"
    case _ => "other"
