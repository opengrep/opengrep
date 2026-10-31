// The value of an indented body that ends with an end marker is its last
// expression.
object A:
  def h(c: Boolean) =
    val y =
      if c then
        source()
      else
        source()
      end if
    // ruleid: taint-scala-end-marker-value
    sink(y)

  def g(x: Int) =
    val z =
      x match
        case 1 => source()
        case _ => source()
      end match
    // ruleid: taint-scala-end-marker-value
    sink(z)

  def k(c: Boolean) =
    val w =
      g()
      if c then
        source()
      else
        source()
      end if
    // ruleid: taint-scala-end-marker-value
    sink(w)
