function test_builtin_reduceRight() {
  const arr = [source()];
  arr.reduceRight((acc, x) => {
    // ruleid: test-hof-taint
    sink(x);
    return acc;
  }, []);
}
