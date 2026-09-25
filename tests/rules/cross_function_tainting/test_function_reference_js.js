function toSink(a) {
  // ruleid: test-function-reference-js
  sink(a);
}

function viaVariable() {
  const f = toSink;
  f(source());
}

function sinkShadowedByLocal(a) {
  // ok: test-function-reference-js
  sink(a);
}

function localValueShadowsFunction() {
  let sinkShadowedByLocal = 0;
  const f = sinkShadowedByLocal;
  f(source());
}
