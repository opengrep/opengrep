function caller_array() {
  const lambdas = [
    (x) => {
      const r = [];
      r.push(x);
      return r;
    },
  ];
  const fn = lambdas[0];
  const result = fn(getInput());
  // ruleid: test-builtin-in-lambda-signature
  sink(result);
}
