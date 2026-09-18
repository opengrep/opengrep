function test_builtin_forEach() {
  const arr = [source()];
  arr.forEach((x) => {
    // ruleid: test-hof-taint
    sink(x);
  });
}
