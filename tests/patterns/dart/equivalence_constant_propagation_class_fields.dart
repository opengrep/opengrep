class A {
  String _f1;
  final String _f3 = "abc";
  String _f4;
  String f5;
  final String _f7;
  String _f8 = "abc";
  static String _f2 = "abc";
  A() { _f1 = "abc"; _f4 = "abc"; f5 = "abc"; }
  A.named() : _f7 = "abc" { _f1 = "zzz"; }
  void other() { _f4 = "zzz"; }
  void m() {
    // ERROR: match
    var loc = "abc"; sink(0, loc);
    sink(1, _f1);
    sink(2, _f2);
    // ERROR: match
    sink(3, _f3);
    sink(4, _f4);
    sink(5, f5);
    sink(8, _f8);
  }
}
class B {
  String _g1;
  B() { _g1 = "abc"; }
  void m() { sink(10, _g1); }
}
