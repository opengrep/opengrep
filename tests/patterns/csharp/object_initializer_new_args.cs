class Test {
  void F(string v) {
    // ERROR:
    var a = new Foo { X = v };
    // ERROR:
    var b = new Foo();
    // ERROR:
    var c = new Foo(v) { X = v };
    var d = new Bar { X = v };
  }
}
