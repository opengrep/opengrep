// An unknown method called on a tainted value that a lambda captures returns
// the taint of that value.
function main() {
  const s = source();
  const c = "safe";
  // ruleid: js_method_call_on_captured_tainted_value
  [1].forEach(() => sink(s.trim()));
  // ok: js_method_call_on_captured_tainted_value
  [1].forEach(() => sink(c.trim()));
}
