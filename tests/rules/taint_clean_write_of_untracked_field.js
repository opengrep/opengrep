function run() {
  const o = {};
  o.t = source();
  o.f1 = "c";
  o.f2 = "c";
  o.f3 = "c";
  o.f4 = "c";
  o.f5 = "c";
  o.f6 = "c";
  o.f7 = "c";
  o.f8 = "c";
  o.f9 = "c";
  o.f10 = "c";
  o.f11 = "c";
  // ruleid: taint-clean-write-of-untracked-field
  sink(o.t);
  // ok: taint-clean-write-of-untracked-field
  sink(o.f1);
}
