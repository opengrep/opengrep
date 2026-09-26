function orDefault(list, v) {
  list = list || [];
  list.push(v);
}

function fresh(list, v) {
  list = [];
  list.push(v);
}

function onePath(list, v, c) {
  if (c) {
    list = [];
  }
  list.push(v);
}

function t1() {
  var l = [];
  orDefault(l, source());
  // ruleid: test-param-rebinding-per-path-js
  sink(l);
}

function t2() {
  var l = [];
  fresh(l, source());
  // ok: test-param-rebinding-per-path-js
  sink(l);
}

function t3(c) {
  var l = [];
  onePath(l, source(), c);
  // ruleid: test-param-rebinding-per-path-js
  sink(l);
}
