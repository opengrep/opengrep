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

  void guardAmongLabelsFails() {
    var x = taint_source();
    object k = 3;
    switch (k) {
      case 1:
      case int n when n > 5:
        // ok: taint-switch-case-csharp
        sink(x);
        break;
    }
  }

  void guardAmongLabelsHolds() {
    var x = taint_source();
    object k = 7;
    switch (k) {
      case 1:
      case int n when n > 5:
        // ruleid: taint-switch-case-csharp
        sink(x);
        break;
    }
  }
}
