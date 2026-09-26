function apply(f, x) {
  return f(x);
}

function clean(v) {
  return "clean";
}

function unknownCallback() {
  const y = apply(unknownFn, source());
  // ruleid: test-unknown-callback-result-js
  sink(y);
}

function cleanCallback() {
  const y = apply(clean, source());
  // ok: test-unknown-callback-result-js
  sink(y);
}
