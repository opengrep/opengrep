function test_custom_foreach() {
  const arr = [source()];
  customForEach(arr, (x) => {
    // ruleid: test-hof-taint
    sink(x);
  });
}
