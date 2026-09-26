function make(v) {
  try {
    risky();
  } catch (e) {
    e.data = v;
    return () => e.data;
  }
}

function sameCall() {
  const b = make(source());
  // ruleid: test-catch-variable-captured-js
  sink(b());
}

function otherCall() {
  const a = make(source());
  const b = make("clean");
  // ok: test-catch-variable-captured-js
  sink(b());
}
