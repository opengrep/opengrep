function test_builtin_find() {
  const arr = [source()];
  arr.find((x) => {
    // ruleid: test-hof-taint
    sink(x);
    return true;
  });
}
