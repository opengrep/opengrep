class Safe {
  void hello() {}
}
// MATCH:
class Risky {
  void hello() {}
  String danger(String s) { return s; }
}
