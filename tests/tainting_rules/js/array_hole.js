// a skipped element keeps the later elements at their index

function t1() {
  const [, b] = [source(), "safe"];
  //OK: test-array-hole
  sink(b);
}

function t2() {
  const [, b] = ["safe", source()];
  //ruleid: test-array-hole
  sink(b);
}

function t3() {
  let y;
  [, y] = [source(), "safe"];
  //OK: test-array-hole
  sink(y);
}

function t4() {
  let y;
  [, , y] = ["safe", "safe", source()];
  //ruleid: test-array-hole
  sink(y);
}
