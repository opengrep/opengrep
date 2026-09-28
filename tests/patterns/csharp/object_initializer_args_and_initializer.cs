class Test {
  void F(string v) {
    // ERROR:
    var a = new Foo { X = v };
    // ERROR:
    var b = new Foo(v) { X = v };
    // ERROR:
    var c = new Foo();
    // ERROR:
    var d = new Foo(v);
    var e = new Bar(v);
  }
}
