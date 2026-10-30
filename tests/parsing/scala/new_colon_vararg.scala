// A colon after `new A` at the end of a line before no indented line is an
// ascription, not an empty body.
object O {
  val a = f(new A:
_*)
}
