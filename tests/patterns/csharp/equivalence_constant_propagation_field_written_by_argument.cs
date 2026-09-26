class A {
  private string f1 = "abc";
  private string f2 = "abc";
  private string f3 = "abc";
  private string f4 = "abc";
  string f5 = "abc";
  void Init() {
    Get(out f1);
    Swap(ref f2);
    Get(out this.f3);
  }
  void M() {
    sink(1, f1);
    sink(2, f2);
    sink(3, f3);
    // ERROR: match
    sink(4, f4);
    // ERROR: match
    sink(5, f5);
  }
  static void Get(out string p) { p = "zzz"; }
  static void Swap(ref string p) { p = "zzz"; }
}
