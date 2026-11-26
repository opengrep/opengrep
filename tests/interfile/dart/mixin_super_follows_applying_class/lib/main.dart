void sink(String x) {
  print(x);
}

String source() {
  return 'tainted';
}

class B {
  void f(String x) {}
}

class A extends B {
  void f(String x) {
    // ruleid: mixin-super-follows-applying-class
    sink(x);
  }
}

mixin M on B {
  void f(String x) {
    super.f(x);
  }
}

class C extends A with M {}

void main() {
  C().f(source());
}
