object Pairs:
  def pair[A](x: A)[B](y: B): (A, B) = (x, y)
  def lift(n: Int)[F[_]](using m: Monad[F]): F[Int] = m.pure(n)
  def g = 1
