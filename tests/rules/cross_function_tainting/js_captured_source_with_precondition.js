function make(x) {
  const t = wrap(x);
  return () => t;
}

function top() {
  // ruleid: captured-source-with-precondition
  sink(make(sourceB())());
  // ok: captured-source-with-precondition
  sink(make("c")());
}
