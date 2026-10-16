class SwitchCase {
  void guardedLabelKeepsLaterCases(int k, bool c) {
    var x = taint_source();
    switch (k) {
      case FOO when c:
        break;
      case BAR:
        // ruleid: taint-switch-case-csharp
        sink(x);
        break;
    }
  }

  int labelIsNotBound() {
    var k = taint_source();
    return k switch {
      // ok: taint-switch-case-csharp
      FOO => sink(FOO),
      _ => 0,
    };
  }
}
