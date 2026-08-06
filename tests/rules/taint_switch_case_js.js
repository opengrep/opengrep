// Regression guard for the `expr_to_pattern` fallback in AST_to_IL.
//
// A `case` label reaches `pattern` as an OtherPat("ExprToPattern", ...) when
// it is not a plain name or literal, exactly like a destructuring assignment
// target does. It is a value to compare the scrutinee against, though, not a
// binding target: lowering `case Colors.RED:` as `Colors.RED = x` would make
// every constant used as a case label inherit the scrutinee's taint, which is
// why the lval binding is gated on pattern_binds_lvals.
function caseLabelIsNotABindingTarget() {
  var x = taint_source();
  switch (x) {
    case Colors.RED:
      break;
    default:
      break;
  }

  // ok: taint-switch-case-js
  sink(Colors.RED);

  // ok: taint-switch-case-js
  sink(Colors);
}

// Taint out of the scrutinee itself is unaffected.
function scrutineeStillFlows() {
  var y = taint_source();
  switch (y) {
    case 1:
      // ruleid: taint-switch-case-js
      sink(y);
      break;
  }
}
