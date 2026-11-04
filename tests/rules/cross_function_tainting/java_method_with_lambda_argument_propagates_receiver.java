class C {
  void tainted() {
    String s = source();
    String t = s.transform(x -> x);
    // ruleid: java_method_with_lambda_argument_propagates_receiver
    sink(t);
  }

  void clean() {
    String s = "fixed";
    String t = s.transform(x -> x);
    // ok: java_method_with_lambda_argument_propagates_receiver
    sink(t);
  }
}
