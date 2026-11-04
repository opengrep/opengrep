// A lambda that calls a parameter of the function that defines it calls the
// callback the caller passes, not an unknown function.
function apply(k, x, y) {
  const g = () => k(y, x);
  return g();
}
function main() {
  const first = (p, q) => p;
  // ruleid: js_call_through_captured_parameter_is_a_callback_call
  sink(apply(first, "safe", source()));
  // ok: js_call_through_captured_parameter_is_a_callback_call
  sink(apply(first, source(), "safe"));
}
