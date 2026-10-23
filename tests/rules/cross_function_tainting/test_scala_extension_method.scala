object Extensions:
  extension (s: String)
    def leak(): Unit =
      // ruleid: scala_extension_method_receiver
      sink(s)

  extension (n: Int)
    def report(): Unit =
      // ok: scala_extension_method_receiver
      sink(n)

  def tainted(): Unit =
    source().leak()

  def clean(): Unit =
    1.report()
