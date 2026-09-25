function toSink(a) {
  // ruleid: test-function-reference-js
  sink(a);
}

function viaObject() {
  const handlers = { k: toSink };
  handlers.k(source());
}
