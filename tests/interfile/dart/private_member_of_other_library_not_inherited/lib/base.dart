void sink(String x) {
  print(x);
}

class Base {
  void _leak(String x) {
    // ok: private-member-of-other-library-not-inherited
    sink(x);
  }
}
