// The type parameters of a later clause join the first clause.
object Pairs:
  //ERROR:
  def pair[A](x: A)[B](y: B): (A, B) = (x, y)

  //ERROR:
  def same[A, B](x: A)(y: B): (A, B) = (x, y)

  //OK:
  def single[A](x: A)(y: Int): (A, Int) = (x, y)
