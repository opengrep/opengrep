object Foo:
  extension (x: Int)
    def double =
      //ERROR:
      sink(x)
    end double
    def triple =
      //OK:
      log(x)
