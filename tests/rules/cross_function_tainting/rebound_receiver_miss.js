class Foo {
  run(x) {
    // ruleid: rebound_receiver_miss
    sink(x);
  }
}
class Bar {
  run(x) {}
}
function test() {
  var c = new Bar();
  c.run(clean());
  c = new Foo();
  c.run(source());
}
