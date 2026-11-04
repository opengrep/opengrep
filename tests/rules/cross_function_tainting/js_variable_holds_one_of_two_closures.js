// A variable that holds one of two closures of different lambdas calls
// either of them.
function mk(v) {
  return () => v;
}
function mkb(v) {
  return () => v + "b";
}
function main(c) {
  let a = mkb("x");
  if (c) {
    a = mk(source());
  }
  // ruleid: js_variable_holds_one_of_two_closures
  sink(a());
}
function control(c) {
  let a = mkb("x");
  if (c) {
    a = mk("y");
  }
  // ok: js_variable_holds_one_of_two_closures
  sink(a());
}
