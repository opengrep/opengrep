// An assignment that starts the template body of a given is not the type of
// the given.
object A:
  given Ord[Long]:
    //ERROR:
    count = 1
    def other(v: Long): Unit = ()
