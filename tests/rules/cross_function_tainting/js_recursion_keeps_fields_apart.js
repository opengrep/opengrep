// A recursive function that returns one field of its parameter returns that
// field, not the other fields of the argument.
function get(n, o) {
  if (n > 0) { return get(n - 1, o); }
  return o.b;
}
function main() {
  var tainted = {};
  tainted.b = source();
  // ruleid: js_recursion_keeps_fields_apart
  sink(get(3, tainted));
  var other = {};
  other.a = source();
  // ok: js_recursion_keeps_fields_apart
  sink(get(3, other));
}
