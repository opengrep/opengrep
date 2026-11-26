function shadowed() {
  function helper(x) {
    // ok: local-shadows-called-function
    sink(x);
  }
  function inner() {
    const helper = other();
    return helper(source());
  }
  return inner;
}

function called() {
  function helper(x) {
    // ruleid: local-shadows-called-function
    sink(x);
  }
  function inner() {
    return helper(source());
  }
  return inner;
}
