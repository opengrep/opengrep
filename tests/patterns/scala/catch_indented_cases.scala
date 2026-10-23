object Foo:
  def f() =
    //ERROR:
    try g()
    catch
      case e: IllegalStateException => 0
      case e: RuntimeException => 1

  def h() =
    //OK:
    try g()
    catch
      case e: IllegalStateException => 0
