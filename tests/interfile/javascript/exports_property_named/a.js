function run(x) {
  // ruleid: exports-property-named
  sink(x);
}
function hidden(x) {
  // ok: exports-property-named
  sink(x);
}
exports.run = run;
