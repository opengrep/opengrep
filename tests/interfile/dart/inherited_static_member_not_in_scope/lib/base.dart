void sink(String x) {
  print(x);
}

class Base {
  static void leak(String x) {
    // ok: inherited-static-member-not-in-scope
    sink(x);
  }
}
