// Receiver-element through HOF dispatch: unknownFn(getInput()) carries
// the source's xtaint into arr; arr.forEach invokes the lambda with
// each element carrying that xtaint; the lambda pushes into a
// captured-local b; sink(b) sees the propagated taint. The chain
// requires both the lambda's signature to record its push as a
// ToLval on a captured local, and HOF dispatch to flow that ToLval
// from the callback to the surrounding call site.

function f() {
  const arr = unknownFn(getInput());
  const b = [];
  arr.forEach(elem => b.push(elem));
  // ruleid: test-hof-push-js
  sink(b);
}
