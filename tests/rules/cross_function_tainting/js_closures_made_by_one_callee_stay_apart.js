// Two closures of one lambda that one callee makes with two calls keep their
// own values in the callee's caller.
function mk(v) {
  return () => v;
}
function two(a, b) {
  const f = mk(a);
  const g = mk(b);
  return [f, g];
}
function main() {
  const p = two(source(), "safe");
  // ruleid: js_closures_made_by_one_callee_stay_apart
  sink(p[0]());
  // ok: js_closures_made_by_one_callee_stay_apart
  sink(p[1]());
}
