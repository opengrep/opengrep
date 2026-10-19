trait Ord[T]

object Foo:
  //ERROR:
  given [T: Ord] => Ord[List[T]] = sink()

  //OK:
  given Ord[Int] = log()
