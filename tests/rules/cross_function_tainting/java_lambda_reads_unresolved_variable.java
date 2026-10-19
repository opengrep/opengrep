class A {
  // A variable of a [for] initialiser, which naming does not resolve, read by
  // a lambda: the lambda captures it.
  void tainted() {
    for (String s = source(); s != null; ) {
      // ruleid: java_lambda_reads_unresolved_variable
      Runnable g = () -> sink(s);
      g.run();
      break;
    }
  }

  void untainted() {
    for (String s = clean(); s != null; ) {
      // ok: java_lambda_reads_unresolved_variable
      Runnable g = () -> sink(s);
      g.run();
      break;
    }
  }
}
