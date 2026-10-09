// A method called on a field of the receiver reads that field's object, not
// the receiver.
class Child {
  read() {
    // ok: lambda-sig-js-field-receiver
    sink(this.secret);
  }
}
class Outer {
  constructor() {
    this.secret = source();
    this.child = new Child();
  }
  go() {
    this.child.read();
  }
  own() {
    // ruleid: lambda-sig-js-field-receiver
    sink(this.secret);
  }
}
function run() {
  const o = new Outer();
  o.go();
  o.own();
}
