// `{` on the line after `if (c)`, at the column of the `if`, starts the body
// of the `if`; the statement after the body is not part of it.
object A {
  def f(x: String) = {
    //ERROR:
    if (x != null)
    {
      sink(x)
    }
    g()
  }

  def h(x: String) = {
    //OK:
    if (x != null)
    {
      sink(x)
      g()
    }
  }
}
