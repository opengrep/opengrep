function not_a_hof(x) {
  function callback(y) { return 3; }
  return callback(x);
}

function propagates(x) { return x; }

function real_hof(callback, value) {
  return callback(value);
}

function test_real_hof() {
  // ruleid: taint-hof-parameter-shadowed-by-nested-javascript
  sink(real_hof(propagates, source()));
}
