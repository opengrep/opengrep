function caller() {
  const t = getInput();
  const r = helper(t);
  const { b } = r;
  // ruleid: test-maybe-doubleedge-js
  sink(b);
}
