class TestParamRebinding {
  static void rebind(String ctx, String x) {
    ctx = x;
    use(ctx);
  }

  static void caller() {
    String c = "safe";
    rebind(c, source());
    // ok: test-param-rebinding-java
    sink(c);
  }
}
