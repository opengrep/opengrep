function test_custom_map() {
  const arr = [source()];
  customMap(arr, (x) => {
    // ruleid: test-hof-taint
    sink(x);
  });
}
