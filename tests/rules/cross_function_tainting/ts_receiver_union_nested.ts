class A {
  run(): string {
    return source();
  }
}

class B {
  run(): string {
    return "safe";
  }
}

function outerA(x: A | undefined) {
  function inner(x: B | undefined) {
    // ok: ts_receiver_union_nested
    sink(x.run());
  }
  // ruleid: ts_receiver_union_nested
  sink(x.run());
  return inner;
}

function outerB(x: B | undefined) {
  function inner(x: A | undefined) {
    // ruleid: ts_receiver_union_nested
    sink(x.run());
  }
  // ok: ts_receiver_union_nested
  sink(x.run());
  return inner;
}
