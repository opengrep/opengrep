// A returned object with as many fields as an object may hold keeps them, and
// a wider one keeps the fields known clean.
function exact(x) {
  var o = {};
  o.f = {a: x, b0: 0, b1: 1, b2: 2, b3: 3, b4: 4, b5: 5, b6: 6, b7: 7, b8: 8};
  return o;
}
function wide(x) {
  var o = {};
  o.f = {a: x, b0: 0, b1: 1, b2: 2, b3: 3, b4: 4, b5: 5, b6: 6, b7: 7, b8: 8, b9: 9, b10: 10};
  return o;
}
function main() {
  var r = exact(source());
  // ok: js_wide_object_keeps_clean_fields
  sink(r.f.b0);
  // ruleid: js_wide_object_keeps_clean_fields
  sink(r.f.a);
  var w = wide(source());
  // ok: js_wide_object_keeps_clean_fields
  sink(w.f.b0);
  // ruleid: js_wide_object_keeps_clean_fields
  sink(w.f.a);
}
