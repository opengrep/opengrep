class Box {}

class Crate {}

// ERROR:
extension Loud on Box {
  void handle(String x) {}
}

extension Quiet on Crate {
  void handle(String x) {}
}
