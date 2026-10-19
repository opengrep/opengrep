// An alias given is a final lazy variable.
object Foo:
  //ERROR:
  given ec: ExecutionContext = makeContext()

  //OK:
  val other: ExecutionContext = makeContext()
