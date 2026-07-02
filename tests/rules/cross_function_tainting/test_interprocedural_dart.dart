String helperSource() {
  return source();
}

void helperToSink(String x) {
  // ruleid: test-interprocedural-dart
  sink(x);
}

String helperFlow(String x) {
  var y = x;
  return y;
}

String nestedFlow(String input) {
  String inner(String v) {
    return v;
  }

  return inner(input);
}

void main() {
  var input = helperSource();
  var x = helperFlow(input);
  helperToSink(x);
  // ruleid: test-interprocedural-dart
  sink(nestedFlow(input));
}

void mainClean() {
  var x = helperFlow("clean");
  sink(x);
  sink(nestedFlow("clean"));
}
