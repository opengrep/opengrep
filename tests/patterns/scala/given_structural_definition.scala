trait Ord[T]

// A structural given is an object that extends its type.
object Foo:
  //ERROR:
  given intOrd: Ord[Int] with
    def compare(a: Int, b: Int) = a - b

  //OK:
  object plainOrd extends Ord[Long]
