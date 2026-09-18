function caller_record() {
  const lambdas = {
    do_taint: (x) => {
      const r = [];
      r.push(x);
      return r;
    },
  };
  const result = lambdas.do_taint(getInput());
  // ruleid: test-builtin-in-lambda-signature
  sink(result);
}
