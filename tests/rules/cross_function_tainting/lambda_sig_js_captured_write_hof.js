// A callback a built-in calls writes a captured local, read after the call.
function collect(items) {
  const out = [];
  items.forEach((item) => {
    out.push(item);
  });
  return out;
}

function go() {
  const res = collect([source()]);
  // ruleid: lambda-sig-js-captured-write-hof
  sink(res);
  // ok: lambda-sig-js-captured-write-hof
  sink(collect(["a"]));
}
