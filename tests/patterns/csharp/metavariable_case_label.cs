class C {
  void f(object o) {
    // ERROR: match
    var a = o is FOO;
    // ERROR: match
    var b = o is null;
    // ERROR: match
    var c = o is string s;
    // ERROR: match
    var d = o is var v;
    // ERROR: match
    var e = o is > 5;
  }
}
