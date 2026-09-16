function not_a_hof(x) {
  function callback(y) { return 3; }
  return callback(x);
}

function propagates(x) { return x; }

function real_hof(value) {
  const callback = propagates;
  return callback(value);
}

function test_real_hof() {
  // ruleid: taint-hof-local-shadowed-by-nested-javascript
  sink(real_hof(source()));
}
