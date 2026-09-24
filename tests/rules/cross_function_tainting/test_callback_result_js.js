function apply(f) {
  return f();
}

function callOnly(f) {
  f();
  return "clean";
}

function sinkIt(f) {
  // ruleid: test-callback-result-js
  sink(f());
}

function store(box, f) {
  box.v = f();
}

function wrap(handler) {
  return (c) => {
    const res = handler(c);
    use(res);
  };
}

function tainted() {
  return source();
}

function test() {
  // ruleid: test-callback-result-js
  sink(apply(tainted));
  // ok: test-callback-result-js
  sink(callOnly(tainted));
  sinkIt(tainted);
  const box = {};
  store(box, tainted);
  // ruleid: test-callback-result-js
  sink(box.v);
  // ok: test-callback-result-js
  sink(wrap(tainted));
}
