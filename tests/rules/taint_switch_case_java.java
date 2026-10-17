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

  void defaultFirst(int k) {
    String x = taint_source();
    switch (k) {
      default:
        // ruleid: taint-switch-case-java
        sink(x);
        break;
      case 1:
        break;
    }
  }

  void switchRuleDoesNotFallThrough(int k) {
    String y = "";
    switch (k) {
      default -> y = taint_source();
      // ok: taint-switch-case-java
      case 1 -> sink(y);
    }
  }

  void lastSwitchRuleIsNotReachedByFallthrough(int k) {
    String y = "";
    switch (k) {
      case 1 -> y = taint_source();
      // ok: taint-switch-case-java
      default -> sink(y);
    }
  }

  void switchExpressionWithDefaultFirst(int k) {
    String r = switch (k) {
      default -> taint_source();
      case 1 -> "";
    };
    // ruleid: taint-switch-case-java
    sink(r);
  }
}
