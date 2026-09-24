class A {
  private String f1;
  private static String s1;
  private String f2;
  private String f3;
  { f2 = "abc"; }
  A() { s1 = "abc"; Runnable r = () -> { f3 = "abc"; }; }
  void m() {
    sink(1, f1);
    // ERROR: match
    sink(2, f2);
    sink(3, f3);
    sink(4, s1);
    sink(5, this.s1);
  }
  class Inner {
    void n() { sink(6, f3); }
  }
}
class B {
  private String g1;
  void B() { g1 = "abc"; }
  void m() { sink(7, g1); }
}
class C {
  private String h1;
  private String h2;
  C() { h1 = "abc"; }
  C(int x) { this(); }
  void m() { sink(8, h1); }
}
