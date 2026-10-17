class C {
  void f(object o) {
    // ERROR: match
    var r1 = o is (FOO, var y);
    // ERROR: match
    var r2 = o is (FOO, 1, 2);
    var r3 = o is (a: FOO, b: var y);
    var r4 = o is (BAR, FOO);
  }
}
