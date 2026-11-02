function make() {
  const t = source();
  return () => t;
}

function makeObject() {
  const o = { a: source(), b: "c" };
  return () => o;
}

function top() {
  // ruleid: returned-lambda-carries-captured-source
  sink(make()());
  const r = makeObject()();
  // ruleid: returned-lambda-carries-captured-source
  sink(r.a);
  // ok: returned-lambda-carries-captured-source
  sink(r.b);
}
