// A closure that a recursive call returns writes to the variable of that
// call, not to the variable of the same name of the caller.
function mk(o, n) {
  if (n > 0) {
    const s = mk({}, n - 1);
    s();
    // ok: js_closure_of_a_recursive_call_writes_its_own_variable
    sink(o.x);
  }
  return () => { o.x = source(); };
}
function main() {
  const o = {};
  const s = mk(o, 0);
  s();
  // ruleid: js_closure_of_a_recursive_call_writes_its_own_variable
  sink(o.x);
}
