// The indentation of a line is the width of the whitespace that starts it:
// a comment before its first token does not change it.
object A:
  //ERROR:
  def f() =
    /* c */ val x = source()
    sink(x)

class B {
  // `apply` is a member, not a statement of the body of `isDefinedAt`
  //OK:
  def isDefinedAt(x: Int): Boolean =
    check(x)

  /*final*/ def apply(x: Int): Int = sink(x)
}

class C {
  // the spaces after a comment at the start of a line are not indentation
  //OK:
  def isDefinedAt(x: Int): Boolean =
    check(x)
/*final*/      def apply(x: Int): Int = sink(x)
}

object D:
  // the line of `sink` starts in a comment over two lines, whose last line
  // gives its indentation
  //ERROR:
  def f() =
    val x = source()
  /* c
    */ sink(x)
