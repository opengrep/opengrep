// Two closures of one lambda, created by two calls, hold their own values.
function mk(v) {
  return () => v;
}

function main() {
  const a = mk(source());
  const b = mk("safe");
  // ok: js_closures_of_one_lambda_from_two_calls
  sink(b());
  // ruleid: js_closures_of_one_lambda_from_two_calls
  sink(a());
}
