class A {
  private String f1;
  A() { f1 = "abc"; }
  static void reset(A other) { other.f1 = "zzz"; }
  void m() { sink(1, f1); }
}
