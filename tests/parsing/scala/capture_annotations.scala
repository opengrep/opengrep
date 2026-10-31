trait Builder[-A, +To] extends Growable[A] { self: Builder[A, To]^ =>
  def addAll(xs: IterableOnce[A]^): this.type
  def view: View[A]^{this} = ???
  def lazyZip[B](that: Iterable[B]^): LazyZip2[A, B, this.type]^{this, that} = ???
}

object Xor:
  type T = Int ^ Long
  def xor(a: Int, b: Int) = a ^ b
