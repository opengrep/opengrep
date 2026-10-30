// `implicit` followed by a soft modifier in a block starts a local
// definition, not an implicit lambda.
object O {
  def g = {
    implicit inline def f: Int = 1
    implicit transparent inline def h: Int = 2
    f + h
  }
}
