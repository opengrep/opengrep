// After an end marker, a less indented line that starts with a selection
// continues the expression whose body the marker ends.
object A:
  def f(x: Int) =
    val y = xs.map: x =>
      try g()
      finally h()
      end try
    .filter(z => z)
    y
