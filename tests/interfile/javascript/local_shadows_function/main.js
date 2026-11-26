function apply(f, v) {
  return f(v);
}

function shadowed(x) {
  // ok: local-shadows-function
  return sink(x);
}

function passed(x) {
  // ruleid: local-shadows-function
  return sink(x);
}

function passesShadowingLocal() {
  var shadowed = { trigger: function () {} };
  return apply(shadowed, source());
}

function passesFunction() {
  return apply(passed, source());
}
