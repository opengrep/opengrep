class A {
  private String f1;
  private static String f2;
  private String f3 = "abc";
  private String f4;
  String f5;
  private String f6;
  static { f2 = "abc"; }
  A() { f1 = "abc"; f4 = "abc"; f5 = "abc"; this.f6 = "abc"; }
  void other() { f4 = "zzz"; }
  void m() {
    // ERROR: match
    sink(1, f1);
    // ERROR: match
    sink(2, f2);
    // ERROR: match
    sink(3, f3);
    sink(4, f4);
    sink(5, f5);
    // ERROR: match
    sink(6, this.f6);
  }
}
