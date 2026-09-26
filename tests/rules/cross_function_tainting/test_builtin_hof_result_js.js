function mapped(ids) {
  const ys = ids.map(id => source());
  // ruleid: test-builtin-hof-result-js
  sink(ys);
}

function mk(id) {
  return source();
}

function mappedNamed(ids) {
  const ys = ids.map(mk);
  // ruleid: test-builtin-hof-result-js
  sink(ys);
}

function mappedFunction(ids) {
  const zs = ids.map(function (id) { return source(); });
  // ruleid: test-builtin-hof-result-js
  sink(zs);
}

function each(ids) {
  const r = ids.forEach(id => source());
  // ok: test-builtin-hof-result-js
  sink(r);
}

function keep(ids) {
  const r = ids.filter(id => source());
  // ok: test-builtin-hof-result-js
  sink(r);
}

function flat(ids) {
  const r = ids.flatMap(id => [source()]);
  // ruleid: test-builtin-hof-result-js
  sink(r);
}

function red(ids) {
  const r = ids.reduce((acc, id) => source(), "");
  // ruleid: test-builtin-hof-result-js
  sink(r);
}

const ws = [1, 2].map(id => source());
// ruleid: test-builtin-hof-result-js
sink(ws);

function mappedUnknown() {
  const ys = [source()].map(unknownFn);
  // ruleid: test-builtin-hof-result-js
  sink(ys);
}

function mappedClean() {
  const ys = [source()].map(x => "clean");
  // ok: test-builtin-hof-result-js
  sink(ys);
}
