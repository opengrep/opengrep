function test_builtin_some() {
  const arr = [source()];
  arr.some((x) => {
    // ruleid: test-hof-taint
    sink(x);
    return true;
  });
}
