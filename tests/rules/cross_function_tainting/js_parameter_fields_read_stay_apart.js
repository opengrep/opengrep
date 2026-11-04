// A function that reads several fields of a parameter returns those fields
// of the argument, not the others.
function five(x) {
  return x.a1 + x.a2 + x.a3 + x.a4 + x.a5;
}
function secret(x) {
  return x.a1 + x.a2 + x.a3 + x.a4 + x.secret;
}
function main() {
  const o = { a1: "s", a2: "s", a3: "s", a4: "s", a5: "s", secret: source() };
  // ok: js_parameter_fields_read_stay_apart
  sink(five(o));
  // ruleid: js_parameter_fields_read_stay_apart
  sink(secret(o));
}
