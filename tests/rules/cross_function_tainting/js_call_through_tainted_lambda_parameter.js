// The result of a call through a tainted parameter of a lambda holds the
// taint of the parameter, as it does in a named function.
function main() {
  run((tainted) => {
    const r = tainted(0);
    // ruleid: js_call_through_tainted_lambda_parameter
    sink(r);
  });
  run((clean) => {
    const r = clean(0);
    // ok: js_call_through_tainted_lambda_parameter
    sink(r);
  });
}
