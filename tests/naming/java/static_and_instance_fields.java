class Store {
  static String s = "";
  String g = "";

  void f() {
    use(s, g);
    s = "a";
    g = "b";
  }
}
