class Test {
  void F(string v) {
    // ERROR:
    var a = new Foo { X = v };
    var b = new Foo(1) { X = v, Y = 2 };
    var c = new Foo(v);
    var d = new List<string> { v };
  }
}
