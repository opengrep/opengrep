object Colours {
  val Seq(Red @ _, Green @ _) = Seq(1, 2)

  def f(x: Option[Int]) = x match {
    case All @ Some(_) => All
    case _ => None
  }
}
