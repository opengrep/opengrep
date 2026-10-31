// Scala 2 layouts that indentation regions must not break.
class Annotated {
  private val e =
    new E(a)
  // an annotation before a modifier ends the indented body
  @volatile private var r: Int = 0
  //ERROR:
  def f() = sink(source())
}

object Misindented {
  def g(c: Boolean) = {
    val y =
      if (c) {
        source()
      }
     else 0
    //ERROR:
    sink(y)
  }
}

class Sub extends Base {
  override def f: Int =
     super
    .f
  //ERROR:
  def h() = sink(source())
}

object IfBodyLeft {
  def f(n: Int): Unit =
     if (n > 0)
    g(n)
  //ERROR:
  def k = sink(source())
}

object AnnotationBlankLine {
  def f = {
    @nowarn

    val r = source()
    //ERROR:
    sink(r)
  }
}

object BodyAfterKeywordLeft {
  def f(xs: List[Int]) =
    for (x <- xs) yield
  {
      //ERROR:
      sink(source())
    }

  var x = 0
  def g(c: Boolean) =
    x =
  source()
  //ERROR:
  def k = sink(x)
}
