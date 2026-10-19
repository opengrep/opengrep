object SemicolonBeforeElse {
  def sameLine(x: Boolean) = if (x) 1; else 2

  def nextLine(name: String) =
    if (name == "a") find(name);
    else other(name)

  def inCase(x: Any) = x match {
    case _ => if (x.isInstanceOf[String]) "s"; else x.toString()
  }
}
