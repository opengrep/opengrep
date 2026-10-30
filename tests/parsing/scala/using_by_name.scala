// A using parameter without a name can be by-name.
object A:
  def f(using => Int) = 1
  def g(using Int, => Codec[Int]) = 2
