function apply(f, x) {
  return f(x);
}

function unknownCallback() {
  const y = apply(unknownFn, source());
  // ok: test-unknown-callback-safe-functions-js
  sink(y);
}

function unknownCallbackOfMap() {
  const ys = [source()].map(unknownFn);
  // ok: test-unknown-callback-safe-functions-js
  sink(ys);
}

function directUnknownCall() {
  const y = unknownFn(source());
  // ok: test-unknown-callback-safe-functions-js
  sink(y);
}
