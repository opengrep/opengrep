let x = "";

class A {
  static x = "";

  m() {
    x = "a";
    return x;
  }

  static s() {
    return x;
  }
}
