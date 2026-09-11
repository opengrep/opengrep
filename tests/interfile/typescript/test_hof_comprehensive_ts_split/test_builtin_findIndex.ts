function test_builtin_findIndex() {
  const arr = [source()];
  arr.findIndex((x) => {
    // ruleid: test-hof-taint
    sink(x);
    return true;
  });
}
