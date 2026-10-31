function g(a, unknown, never, infer, out, interface) {
  // MATCH:
  f(a < unknown);
  // MATCH:
  f(a < never);
  // MATCH:
  f(a < infer);
  // MATCH:
  f(a < out.length);
  // MATCH:
  f(a < interface);
  f(a > unknown);
}
