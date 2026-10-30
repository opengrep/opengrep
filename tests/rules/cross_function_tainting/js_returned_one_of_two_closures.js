// A function that returns one of two closures returns both, each with the
// values it captures.
function mkA(v) { return function () { return v; }; }
function mkB(v) { return function () { return 1; }; }
function pick(c, s) {
  if (c) { return mkA(s); }
  return mkB(s);
}
function main(c) {
  // ruleid: js_returned_one_of_two_closures
  sink(pick(c, source())());
  // ok: js_returned_one_of_two_closures
  sink(pick(c, "safe")());
}
