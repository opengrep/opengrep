trait Ord[T]:
  def compare(a: T, b: T): Int

object GivenSip64:
  given Ord[Int]:
    def compare(a: Int, b: Int) = a - b

  given intOrd: Ord[Int]:
    def compare(a: Int, b: Int) = a - b

  given [T: Ord] => Ord[List[T]]:
    def compare(a: List[T], b: List[T]) = 0

  given [A] => AtInstant[Dated[A]] = _.date

  given convert: [A: ClassTag] => Conversion[A, List[A]] = List(_)

  given (ord: Ord[Int]) => Ord[Option[Int]] = ???

  given Int => String = _.toString

  given Ord[Long] {
    def compare(a: Long, b: Long) = 0
  }

  given listOrd[T](using ord: Ord[T]): Ord[List[T]] with
    def compare(a: List[T], b: List[T]) = 0

object UsingConditions:
  given (using ctx: Context) => Ord[Context] = mk(ctx)
  given [T] => (using o: Ord[T]) => Ord[List[T]] = listOrd(o)
