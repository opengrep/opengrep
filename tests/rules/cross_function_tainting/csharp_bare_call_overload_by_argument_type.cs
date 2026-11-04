class A {
  static void F(int i) {
    // ok: bare-call-overload-by-argument-type
    Sink(i);
  }

  static void F(string s) {
    // ruleid: bare-call-overload-by-argument-type
    Sink(s);
  }

  static void Run() {
    string s = Source();
    F(s);
  }
}
