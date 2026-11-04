class A {
  fun f(i: Int) {
    // ok: bare-call-overload-by-argument-type
    sink(i)
  }

  fun f(s: String) {
    // ruleid: bare-call-overload-by-argument-type
    sink(s)
  }

  fun run() {
    val s: String = source()
    f(s)
  }
}
