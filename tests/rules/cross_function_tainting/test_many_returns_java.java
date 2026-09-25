class TestManyReturns {
  static class Svc { String z; }

  static void init(Svc o) {
    if (c()) return;
    if (c()) return;
    if (c()) return;
    o.z = source();
  }

  static void handler() {
    Svc s = new Svc();
    init(s);
    // ruleid: test-many-returns-java
    sink(s.z);
  }

  static void catchAfterCalls() {
    String x = "";
    try {
      a0();
      a1();
      a2();
      a3();
      x = source();
      d();
    } catch (Exception e) {
      // ruleid: test-many-returns-java
      sink(x);
    }
  }
}
