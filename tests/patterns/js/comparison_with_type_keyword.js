function g(a, unknown, never, infer) {
  // MATCH:
  f(a < unknown);
  // MATCH:
  f(a < never);
  // MATCH:
  f(a < infer);
  f(a > unknown);
}
