global class G {
  global static String s1 = 'abc';
  global String s2 = 'abc';
  private static String s3 = 'abc';
  static String s4 = 'abc';
  public void m() {
    sink(1, s1);
    sink(2, s2);
    // ERROR: match
    sink(3, s3);
    // ERROR: match
    sink(4, s4);
  }
}
