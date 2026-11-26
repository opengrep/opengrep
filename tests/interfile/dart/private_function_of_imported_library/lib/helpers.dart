void sink(String x) {
  print(x);
}

void handle(String msg) {
  // ruleid: test-dart-private-function
  sink(msg);
}

void _handle(String msg) {
  // ok: test-dart-private-function
  sink(msg);
}
