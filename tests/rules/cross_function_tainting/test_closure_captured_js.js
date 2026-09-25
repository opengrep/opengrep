function arrowInVariable() {
  const u = source();
  const f = () => u;
  // ruleid: test-closure-captured-js
  sink(f());
}

function taintedAfterCreation() {
  let u = "clean";
  const f = () => u;
  u = source();
  // ruleid: test-closure-captured-js
  sink(f());
}

function withCallback(cb) {
  return cb();
}

function callback(u) {
  // ruleid: test-closure-captured-js
  sink(withCallback(() => u));
}

function mkLocal() {
  const u = source();
  return () => u;
}

function returnedOverLocal() {
  const f = mkLocal();
  // ruleid: test-closure-captured-js
  sink(f());
}

function mkParam(p) {
  return () => p;
}

function returnedOverCleanParam() {
  const g = mkParam("clean");
  // ok: test-closure-captured-js
  sink(g());
}

function main() {
  callback(source());
}
