class I {
  void handle(String x) {
    // ok: implements-gives-no-body
    sink(x);
  }
}

abstract class C implements I {}

class D extends C {
  void handle(String x) {
    // ruleid: implements-gives-no-body
    sink(x);
  }
}

void run(C c) {
  c.handle(source());
}
