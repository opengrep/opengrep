void sink(String x) {
  print(x);
}

void handle(String msg) {
  // ruleid: test-prefixed-import
  sink(msg);
}
