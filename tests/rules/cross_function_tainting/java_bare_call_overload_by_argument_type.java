class A {
  static void f(Integer i) {
    // ok: bare-call-overload-by-argument-type
    sink(i);
  }

  static void f(String s) {
    // ruleid: bare-call-overload-by-argument-type
    sink(s);
  }

  static void run() {
    String s = source();
    f(s);
  }
}
