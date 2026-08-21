function test_builtin_flatMap() {
  const arr = [source()];
  arr.flatMap((x) => {
    // ruleid: test-hof-taint
    sink(x);
  });
}
