// An object with few fields keeps them apart, however many fields its
// fields have.
function ret() {
  return { w: { a: { p: source(), q: 1 }, b: { p: 1, q: 2 }, c: { p: 1, q: 2 }, d: { p: 1, q: 2 } } };
}
function use() {
  const o = ret();
  // ok: js_width_bound_counts_own_fields
  sink(o.w.b.p);
  // ruleid: js_width_bound_counts_own_fields
  sink(o.w.a.p);
}
