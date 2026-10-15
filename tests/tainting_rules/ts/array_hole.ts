// a skipped element keeps the later elements at their index

function t1(): void {
  const [, b]: string[] = [source(), "safe"];
  //OK: test-array-hole
  sink(b);
}

function t2(): void {
  const [, b]: string[] = ["safe", source()];
  //ruleid: test-array-hole
  sink(b);
}
