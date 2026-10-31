object CatchIndentedCases:
  def onOwnLine() =
    try
      g()
    catch
      case e: IllegalStateException => 0
      case e: RuntimeException => 1
    finally
      println("done")

  def atEndOfLine() =
    try g() catch
      case e: IllegalStateException => 0
      case _ => 1

  def oneLine() = try g() catch case e: Exception => 0

  def g(): Int = 1
