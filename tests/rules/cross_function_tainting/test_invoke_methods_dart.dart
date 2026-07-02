// Dart function objects can be invoked explicitly through .call()

void testClosureCall() {
  var f = (String s) {
    // ruleid: test-invoke-methods-dart
    sink(s);
  };
  f.call(source());
}

void testClosureCallFlow() {
  var pass = (String s) {
    return s;
  };
  var out = pass.call(source());
  // ruleid: test-invoke-methods-dart
  sink(out);
}

void testCleanClosureCall() {
  var f = (String s) {
    return s;
  };
  sink(f.call("clean"));
}
