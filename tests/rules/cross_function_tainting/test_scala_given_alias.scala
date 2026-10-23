object Givens:
  given conn: String = source()
  given clean: Int = 1

  def tainted(): Unit =
    // ruleid: scala_given_alias
    sink(conn)

  def untainted(): Unit =
    // ok: scala_given_alias
    sink(clean)
