class Foo {
  run(x) {
    // ok: rebound_receiver_fp
    sink(x);
  }
}
class Bar {
  run(x) {}
}
function test() {
  var c = new Foo();
  c.run(clean());
  c = new Bar();
  c.run(source());
}
