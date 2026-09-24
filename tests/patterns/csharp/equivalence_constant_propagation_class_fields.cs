class A {
  private string f1;
  private static string f2;
  private string f3 = "abc";
  private string f4;
  public string f5;
  string f6;
  private readonly string f7;
  static A() { f2 = "abc"; }
  A() { f1 = "abc"; f4 = "abc"; f5 = "abc"; f6 = "abc"; f7 = "abc"; }
  void Other() { f4 = "zzz"; }
  void M() {
    // ERROR: match
    string loc = "abc"; sink(0, loc);
    // ERROR: match
    sink(1, f1);
    // ERROR: match
    sink(2, f2);
    // ERROR: match
    sink(3, f3);
    sink(4, f4);
    sink(5, f5);
    // ERROR: match
    sink(6, f6);
    // ERROR: match
    sink(7, f7);
  }
}
