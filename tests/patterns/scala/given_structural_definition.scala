trait Ord[T]

object Foo:
  //ERROR:
  given intOrd: Ord[Int] with
    def compare(a: Int, b: Int) = a - b

  //OK:
  object plainOrd extends Ord[Int]
