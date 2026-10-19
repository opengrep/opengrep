object Foo:
  //ERROR:
  inline def f[A](using inline z: Show[A]): Show[A] = sink(z)

  //OK:
  inline def g[A](using z: Show[A]): Show[A] = log(z)
