function mkSetter(o) {
  return () => { o.x = source(); };
}

function use(p) {
  mkSetter(p)();
}

function top() {
  const r = {};
  use(r);
  // ruleid: closure-writes-through-parameter-of-caller
  sink(r.x);
  // ok: closure-writes-through-parameter-of-caller
  sink(r.y);
}
