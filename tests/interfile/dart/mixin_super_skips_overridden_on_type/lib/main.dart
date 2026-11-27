void sink(String x) {
  print(x);
}

String source() {
  return 'tainted';
}

class B {
  void f(String x) {
    // ok: mixin-super-skips-overridden-on-type
    sink(x);
  }
}

class A extends B {
  void f(String x) {}
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
