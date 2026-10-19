object Scala2KeywordIdentifiers {
  def enumIdentifier(x: Option[String]) = x match {
    case Some(enum) => enum
    case None => ""
  }

  private def closeHtmlTags(inline: Inline): Inline = inline

  inline def f(inline x: Int): Int = x

  def enumOperand(enum: Int) = {
    val a = enum max 1
    enum + 1
  }

  enum Color { case Red, Green }
}
