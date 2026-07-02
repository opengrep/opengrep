function test_builtin_filter() {
  const arr = [source()];
  arr.filter((x) => {
    // ruleid: test-hof-taint
    sink(x);
    return true;
  });
}
