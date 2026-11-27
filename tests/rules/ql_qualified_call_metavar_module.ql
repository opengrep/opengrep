predicate f(int a, int b) {
  // ruleid: ql-qualified-call-metavar-module
  DataFlow::localFlow(a, b)
  or
  // ok: ql-qualified-call-metavar-module
  TaintTracking::localFlow(a, b)
}
