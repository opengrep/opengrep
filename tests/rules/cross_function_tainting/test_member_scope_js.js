let x = "";

class A {
  constructor() {
    this.x = "";
  }
  write() {
    x = source();
  }
  readField() {
    return this.x;
  }
}

function test() {
  const a = new A();
  a.write();
  // ok: test-member-scope-js
  sink(a.readField());
  // ruleid: test-member-scope-js
  sink(x);
}
