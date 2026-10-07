// Qualified and bare case labels read values, without binding the
// scrutinee to the constant or enum member.
class SwitchCase {
  static final int CMD = 7;
  void test(int mode) {
    // ruleid: switch-case-constant-propagation-java
    exec(CMD);
    switch (mode) {
      case CMD:
        // ruleid: switch-case-constant-propagation-java
        exec(CMD);
        break;
    }
    // ruleid: switch-case-constant-propagation-java
    exec(CMD);
  }

  void caseLabelIsNotABindingTarget() {
    String x = taint_source();
    switch (x) {
      case Colors.RED:
        break;
      default:
        break;
    }

    // ok: taint-switch-case-java
    sink(Colors.RED);
  }

  void scrutineeStillFlows() {
    String y = taint_source();
    switch (y) {
      case "a":
        // ruleid: taint-switch-case-java
        sink(y);
        break;
    }
  }

  // A bare name labels the case (an enum member): it is compared with the
  // scrutinee and binds nothing, so the later cases stay reachable and
  // their assignments are seen.
  void bareNameCaseKeepsLaterCases(Mode mode) {
    String q = "";
    switch (mode) {
      case A:
        q = "safe";
        break;
      case B:
        q = taint_source();
        // ruleid: taint-switch-case-java
        sink(q);
        break;
    }
  }

  void bareNameCaseIsNotABindingTarget() {
    String y = taint_source();
    switch (y) {
      case RED:
        break;
    }
    // ok: taint-switch-case-java
    sink(RED);
  }
}
