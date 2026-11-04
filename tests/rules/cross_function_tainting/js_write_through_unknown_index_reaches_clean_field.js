// A write through an index the analysis does not know may be to a field
// whose part is known clean.
function written(k) {
  let x = { a: { p: "lit", q: source() } };
  x[k] = { p: source() };
  // ruleid: js_write_through_unknown_index_reaches_clean_field
  sink(x.a.p);
}
function joined(c, k) {
  let x;
  if (c) {
    x = { a: { p: "lit", q: source() } };
  } else {
    x = {};
    x[k] = { p: source() };
  }
  // ruleid: js_write_through_unknown_index_reaches_clean_field
  sink(x.a.p);
}
function rewritten(k) {
  let x = { a: { p: "lit", q: source() } };
  x[k] = { p: source() };
  x.a.p = "lit";
  // ok: js_write_through_unknown_index_reaches_clean_field
  sink(x.a.p);
}
