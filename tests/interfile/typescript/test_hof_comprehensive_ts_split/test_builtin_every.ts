function test_builtin_every() {
  const arr = [source()];
  arr.every((x) => {
    // ruleid: test-hof-taint
    sink(x);
    return true;
  });
}
