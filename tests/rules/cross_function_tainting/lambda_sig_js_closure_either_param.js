// A closure writing a captured variable that holds either of two parameters
// writes both, where the closure leaves the function.
function outer(a, b, c) {
  const o = c ? a : b;
  return () => {
    o.x = source();
  };
}
function main() {
  const p = {};
  const q = {};
  const r = {};
  outer(p, q, r)();
  // ruleid: lambda-sig-js-closure-either-param
  sink(p.x);
  // ruleid: lambda-sig-js-closure-either-param
  sink(q.x);
  // ok: lambda-sig-js-closure-either-param
  sink(r.x);
}
