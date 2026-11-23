class CharFloatOrdering {
  void charOrdering() {
    String x = source();
    char c = 'A';
    if (c >= 'a') {
      // ok: test
      sink(x);
    }
    if (c < 'a') {
      // ruleid: test
      sink(x);
    }
  }

  void floatOrdering() {
    String x = source();
    double d = 2.5;
    if (d < 1.0) {
      // ok: test
      sink(x);
    }
    if (d > 1.0) {
      // ruleid: test
      sink(x);
    }
  }
}
