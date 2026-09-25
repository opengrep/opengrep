type P = { f: string };
interface I { f: string }

function byObjectType(p: P) { p.f = source(); }
function byInterface(i: I) { i.f = source(); }

function caller() {
  const a: P = { f: "" };
  byObjectType(a);
  // ruleid: test-value-copies-ts
  sink(a.f);
  const b: I = { f: "" };
  byInterface(b);
  // ruleid: test-value-copies-ts
  sink(b.f);
}
