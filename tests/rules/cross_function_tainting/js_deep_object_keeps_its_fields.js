// An object written in one assignment keeps the fields of all its levels.
function main() {
  const o = { p: { q: { r: { s: { t: source(), u: "safe" } } } } };
  // ruleid: js_deep_object_keeps_its_fields
  sink(o.p.q.r.s.t);
  // ok: js_deep_object_keeps_its_fields
  sink(o.p.q.r.s.u);
}
