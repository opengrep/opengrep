// Two closures of one lambda, created by two calls in a function that two
// other calls forward, keep their own captured values.
function mk(v) { return () => v; }
function wrap(v, w) { return { a: mk(v), b: mk(w) }; }
function wrap2(v, w) { return wrap(v, w); }
function main() {
  const p = wrap2(source(), "safe");
  // ruleid: js_closures_returned_through_two_calls_stay_apart
  sink(p.a());
  // ok: js_closures_returned_through_two_calls_stay_apart
  sink(p.b());
}
