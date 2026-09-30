class Conv

class Plain

class Wrapped

object P {
  implicit def intToConv(n: Int): Conv = new Conv

  implicit def plainToWrapped(p: Plain): Wrapped = new Wrapped

  implicit def intToText(n: Int): String = ""

  implicit class Rich(val p: Plain) {
    def shout(): String = {
      return source()
    }
  }

  def fromInt(c: Conv): String = {
    return source()
  }

  def fromClass(w: Wrapped): String = {
    return source()
  }

  def betweenBuiltins(s: String): String = {
    return source()
  }

  def toImplicitClass(r: Rich): String = {
    return source()
  }

  def oneOfSeveral(c: Conv): String = {
    return source()
  }

  def oneOfSeveral(b: Boolean): String = {
    return ""
  }

  def clean(c: Conv): String = {
    return ""
  }

  def f(): Unit = {
    val p: Plain = new Plain
    // ruleid: user-defined-conversion
    sink(fromInt(1))
    // ruleid: user-defined-conversion
    sink(fromClass(p))
    // ruleid: user-defined-conversion
    sink(betweenBuiltins(1))
    // ruleid: user-defined-conversion
    sink(toImplicitClass(p))
    // ruleid: user-defined-conversion
    sink(oneOfSeveral(1))
    // The member is selected on Plain, not on the result of the view to
    // Rich (Scala 7.3): member lookup, not overload selection.
    // todoruleid: user-defined-conversion
    sink(p.shout())
    // ok: user-defined-conversion
    sink(clean(1))
  }
}
