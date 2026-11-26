void sink(String x) {
  print(x);
}

class A {
  void f(String x) {
    // ok: test-mixin-linearisation
    sink(x);
  }
}

mixin M1 {
  void f(String x) {
    // ok: test-mixin-linearisation
    sink(x);
  }
}

mixin M2 {
  void f(String x) {
    // ruleid: test-mixin-linearisation
    sink(x);
  }
}
