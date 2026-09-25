partial class A {
  private string f1;
  A() { f1 = "abc"; }
  void M() { sink(1, f1); }
}
partial class A {
  void Other() { f1 = "zzz"; }
}
