void sink(String x) {
  print(x);
}

void handle(String msg) {
  // ok: test-prefixed-import
  sink(msg);
}
