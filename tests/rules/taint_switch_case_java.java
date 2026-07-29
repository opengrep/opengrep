// Same regression guard as taint_switch_case_js.js, for a frontend that
// builds its case labels with AST_generic_helpers.expr_to_pattern too.
// `Colors.RED` is a DotAccess, `Colors.RED` in a case label must not be
// assigned the scrutinee.
class SwitchCase {

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
}
