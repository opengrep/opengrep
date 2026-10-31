// A statement can follow an end marker whose tag is a keyword.
object A:
  lazy val (a, b) =
    val x = source()
    (x, x)
  end val
  def k = sink(a)

  if c then
    g()
  end if
  "x" - {
    sink(b)
  }
