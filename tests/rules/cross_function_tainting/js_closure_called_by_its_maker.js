// A closure called in the function that makes it reads the values it
// captured, not the variables of the current call of that function.
function mk(v, k) {
  if (k) {
    const f = mk("safe", false);
    // ok: js_closure_called_by_its_maker
    sink(f());
  }
  return () => v;
}
mk(source(), true);

function main() {
  const g = mk(source(), false);
  // ruleid: js_closure_called_by_its_maker
  sink(g());
}
