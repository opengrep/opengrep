public class A {
  private String f1;
  private static String f2;
  private String f3 = 'abc';
  private String f4;
  public String f5;
  String f6;
  static { f2 = 'abc'; }
  public A() { f1 = 'abc'; f4 = 'abc'; f5 = 'abc'; f6 = 'abc'; }
  void other() { f4 = 'zzz'; }
  void m() {
    // ERROR: match
    String loc = 'abc'; sink(0, loc);
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
  }
}
