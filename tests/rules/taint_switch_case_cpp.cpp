// Regression guard for the `expr_to_pattern` fallback in AST_to_IL.
//
// A qualified `case` label reaches `pattern` as an
// OtherPat("ExprToPattern", ...) exactly like a destructuring assignment
// target does. It is a value to compare the scrutinee against, though, not a
// binding target: lowering `case Foo::BAR:` as `Foo::BAR = y` would make
// every constant used as a case label inherit the scrutinee's taint.
void caseLabelIsNotABindingTarget() {
  int y = taint_source();
  switch (y) {
    case Foo::BAR:
      break;
    default:
      break;
  }

  // ok: taint-switch-case-cpp
  sink(Foo::BAR);

  // Taint out of the scrutinee itself is unaffected.
  // ruleid: taint-switch-case-cpp
  sink(y);
}
