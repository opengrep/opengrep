// Two nested helpers of the same name are two bindings: no match.
function first() {
  function helper() { return 1; }
  return helper();
}
function second() {
  function helper() { return 2; }
  return helper();
}
