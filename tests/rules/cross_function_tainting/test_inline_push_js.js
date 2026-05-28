// Control: direct parameter source flows through an inline forEach
// lambda's push into a captured-local b, then to sink(b).
// Verifies that direct-param-to-push works through HOF dispatch when
// the source matches the lambda's own parameter (elem inherits p's
// xtaint via unknownFn).

function f(p) {
  const b = [];
  unknownFn(p).forEach(elem => b.push(elem));
  // ruleid: test-inline-push-js
  sink(b);
}
