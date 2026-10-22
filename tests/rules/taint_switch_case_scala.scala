object SwitchCase {
  def casesDoNotFallThrough(k: Int): Unit = {
    var y = 0
    k match {
      case 1 => y = taint_source()
      case _ =>
        // ok: taint-switch-case-scala
        sink(y)
    }
  }

  def laterCaseIsReachable(k: Int): Unit = {
    val x = taint_source()
    k match {
      case 1 => g(k)
      case _ =>
        // ruleid: taint-switch-case-scala
        sink(x)
    }
  }
}
