void sink(String x) {
  print(x);
}

class B {
  void f(String x) {
    // ok: mixin-on-type-not-ahead-of-superclass
    sink(x);
  }
}

class A extends B {
  void f(String x) {}
}

mixin M on B {}
