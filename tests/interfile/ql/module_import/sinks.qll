int sink(int x) { result = x }

int relay(int x) {
  // ruleid: test-ql-import
  result = sink(x)
}

int quiet(int x) {
  // ok: test-ql-import
  result = sink(x)
}
