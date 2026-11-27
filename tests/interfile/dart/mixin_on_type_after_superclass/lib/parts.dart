void sink(String x) {
  print(x);
}

abstract class B {
  void f(String x);
}

class A implements B {
  void f(String x) {
    // ruleid: mixin-on-type-after-superclass
    sink(x);
  }
}

mixin M on B {}
