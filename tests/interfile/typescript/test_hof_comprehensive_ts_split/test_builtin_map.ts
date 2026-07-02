function test_builtin_map() {
  const arr = [source()];
  arr.map((x) => {
    // ruleid: test-hof-taint
    sink(x);
  });
}
