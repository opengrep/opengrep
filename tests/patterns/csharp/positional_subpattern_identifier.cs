class C {
  void f(object o) {
    // ERROR: match
    var r1 = o is (a: FOO, b: var y);
    var r2 = o is (c: FOO, b: var y);
    var r3 = o is (FOO, var y);
  }
}
