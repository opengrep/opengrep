mixin Handler {
  void handle(String x) {
    // ok: test-superclass-is-not-a-mixin
    sink(x);
  }
}
