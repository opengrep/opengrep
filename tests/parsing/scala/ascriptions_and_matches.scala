object AscriptionsAndMatches:
  def annotated(x: Any) = x.hashCode: @unchecked match { case n => n }

  def conditionMatch(arr: Array[Char], last: Int) =
    var i = last
    while (arr(i): @switch) match
        case '.' | ',' => true
        case _ => false
    do i -= 1
    i

  def ascribedBlock(pov: Pov): Fu[Events] = drawable.so:
    pov match
      case p if p.isDraw => draw(p)
      case _ => fuccess(Nil)
    : Fu[Events]
