class Test {
  void F(string v) {
    // ERROR:
    var a = new List<int>();
    // ERROR:
    var b = new Foo { X = v };
    // ERROR:
    var e = new int[] { 1 };
    // ERROR:
    var f = new int[3];
    var c = new Foo(v);
    var d = Create();
  }
}
