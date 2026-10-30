// A closure that writes to the object it captures writes to the caller's
// object, also when a function forwards the object to the closure's maker.
function mkSetter(o) {
  return () => { o.x = source(); };
}
function wrap(o) {
  return mkSetter(o);
}
function main() {
  const o = {};
  const s = wrap(o);
  s();
  // ruleid: js_closure_writes_through_forwarded_parameter
  sink(o.x);
  // ok: js_closure_writes_through_forwarded_parameter
  sink(o.y);
}
