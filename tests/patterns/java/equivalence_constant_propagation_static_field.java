class Foo {
  private static String FIELD;
  private static String NOT_CONSTANT;

  static {
    FIELD = "password";
  }

  void test() {
    //ERROR: match
    foo(FIELD);
  }

  void reassign() {
    NOT_CONSTANT = "password";
    NOT_CONSTANT = "other";
    foo(NOT_CONSTANT);
  }
}
