// Two closures of one lambda created by different calls stay apart however
// many calls return them: the calls nearest their creation tell them apart.
function mk(v) { return () => v; }
function inner(v) { const a = mk(v); const b = mk("s"); return { a: a, b: b }; }
function w1(v) { return inner(v); }
function w2(v) { return w1(v); }
function w3(v) { return w2(v); }
function w4(v) { return w3(v); }
function main() {
  const o = w4(source());
  // ok: js_closures_apart_through_many_calls
  sink(o.b());
  // ruleid: js_closures_apart_through_many_calls
  sink(o.a());
}
