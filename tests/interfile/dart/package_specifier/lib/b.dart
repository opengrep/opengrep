void sink(String x) {
  print(x);
}

void handle(String msg) {
  // ok: test-package-specifier
  sink(msg);
}
