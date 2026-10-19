// Two anonymous lambdas in one function each keep their own signature: a
// temporary's identity is a per-file counter, not its token.
function f(a, b) {
  [1].forEach((x) => {
    // ruleid: lambda-sig-js-two-lambdas
    sink(a);
  });
  [2].forEach((y) => {
    // ok: lambda-sig-js-two-lambdas
    log(y);
  });
  [3].forEach((z) => {
    // ruleid: lambda-sig-js-two-lambdas
    sink(b);
  });
}
function go() {
  f(source(), source());
}
