void sink(String x) {
  print(x);
}

class Base {
  void leak(String x) {
    // ok: library-function-ahead-of-inherited-member
    sink(x);
  }
}
