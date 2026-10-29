object Extensions:
  extension (s: String)
    def leak(): Unit =
      // ruleid: scala_extension_method_receiver
      sink(s)

  extension (n: Int)
    def report(): Unit =
      // ok: scala_extension_method_receiver
      sink(n)

  extension (s: String)(using c: Ctx)
    def pick(x: String): Unit =
      // ruleid: scala_extension_method_receiver
      sink(x)
    def other(x: String): Unit =
      // ok: scala_extension_method_receiver
      sink(c)

  def tainted(): Unit =
    source().leak()

  def clean(): Unit =
    1.report()

  def argument(): Unit =
    "a".pick(source())
    "b".other(source())
