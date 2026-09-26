function mapped(ids) {
  const ys = ids.map(id => source());
  // ruleid: test-target-language-models
  sink(ys);
}

function put(o, v) {
  o.push(v);
}

function pushed() {
  const a = [];
  put(a, source());
  // ruleid: test-target-language-models
  sink(a);
}
