// a hole in an array literal keeps the later elements at their index

function tainted_slot() {
  const a = [1, , source()];
  //ruleid: test-sparse-array-literal
  sink(a[2]);
}

function hole_slot() {
  const a = [1, , source()];
  //OK: test-sparse-array-literal
  sink(a[1]);
}

function leading_hole() {
  const a = [, source()];
  //OK: test-sparse-array-literal
  sink(a[0]);
  //ruleid: test-sparse-array-literal
  sink(a[1]);
}

function middle_hole() {
  const b = [source(), , safe()];
  //OK: test-sparse-array-literal
  sink(b[2]);
  //ruleid: test-sparse-array-literal
  sink(b[0]);
}
