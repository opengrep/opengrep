function update(o, v, a, b, c) {
  if (a) {
    return 1;
  }
  if (b) {
    return 2;
  }
  if (c) {
    return 3;
  }
  o.f = v;
  return 4;
}

function tainted(a, b, c) {
  var o = {};
  update(o, source(), a, b, c);
  // ruleid: js_update_after_early_returns
  sink(o.f);
}

function clean(a, b, c) {
  var o = {};
  update(o, "safe", a, b, c);
  // ok: js_update_after_early_returns
  sink(o.f);
}
