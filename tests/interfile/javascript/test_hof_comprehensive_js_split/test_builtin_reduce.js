function test_builtin_reduce() {
  const arr = [source()];
  arr.reduce((acc, x) => {
    // ruleid: test-hof-taint
    sink(x);
    return acc;
  }, []);
}
