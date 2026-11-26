predicate f(int a, int b) {
  // ERROR:
  DataFlow::localFlow(a, b)
  or
  TaintTracking::localFlow(a, b)
}
