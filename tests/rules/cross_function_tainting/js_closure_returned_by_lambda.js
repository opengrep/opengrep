// A lambda returned by a lambda holds the variables it captured.
function curry(a) {
  return () => () => a;
}
function main() {
  // ruleid: js_closure_returned_by_lambda
  sink(curry(source())()());
  // ok: js_closure_returned_by_lambda
  sink(curry("safe")()());
}
