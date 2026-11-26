void sink(String x) {
  print(x);
}

class Base {
  void leak(String x) {
    // ruleid: test-implicit-member-call
    sink(x);
  }

  void keep(String x) {
    // ok: test-implicit-member-call
    sink(x);
  }
}
