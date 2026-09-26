class I {
  void handle(String x) {
    // ruleid: implements-gives-no-body-control
    sink(x);
  }
}

abstract class C extends I {}

void run(C c) {
  c.handle(source());
}
