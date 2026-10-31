package routes:
  object Foo:
    //ERROR:
    def f = sink(1)

    //OK:
    def g = log(2)
