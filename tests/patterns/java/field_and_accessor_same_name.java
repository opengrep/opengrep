class Person {
  private String name;

  // An accessor named like the field: binding the method must not lose
  // the field's type.
  public String name() {
    return name;
  }

  void greet() {
    //ERROR: match
    foo(name);
  }
}
