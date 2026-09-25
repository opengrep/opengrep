// The constructor tearoff Foo.new refers to the unnamed constructor of Foo,
// which a call through the function value runs.
class Foo {
  Foo(String x) {
    // ruleid: constructor_reference_dart
    sink(x);
  }
}

class Bar {
  Bar(String x) {
    // ok: constructor_reference_dart
    sink(x);
  }
}

Object make(Object Function(String) k) {
  return k(source());
}

void run() {
  make(Foo.new);
}
