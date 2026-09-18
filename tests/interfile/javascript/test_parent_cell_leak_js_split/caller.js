function caller() {
  const r = helper();
  const { b } = r;
  // ok: test-parent-cell-leak-js
  sink(b);
  // ruleid: test-parent-cell-leak-js
  sink(r.a); // positive control: .a carries getInput()'s taint
}
