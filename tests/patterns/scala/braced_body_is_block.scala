// A body in braces is a block, so the three forms match one another.
object Bodies {
  def f(x: String) = {
    //ERROR:
    if (x != null) sink(x)
    //ERROR:
    if (x != null)
      sink(x)
    //ERROR:
    if (x != null) {
      sink(x)
    }
    // a block of two statements is not one statement
    //OK:
    if (x != null) {
      sink(x)
      log(x)
    }
  }
}
