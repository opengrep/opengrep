<?php
// Regression guard for the `expr_to_pattern` fallback in AST_to_IL; see
// taint_switch_case_cpp.cpp. PHP's `Foo::BAR` case label reaches `pattern`
// as an OtherPat("ExprToPattern", ...) and must not be bound to the
// scrutinee.
function caseLabelIsNotABindingTarget() {
  $y = taint_source();
  switch ($y) {
    case Foo::BAR:
      break;
    default:
      break;
  }

  // ok: taint-switch-case-php
  sink(Foo::BAR);

  // Taint out of the scrutinee itself is unaffected.
  // ruleid: taint-switch-case-php
  sink($y);
}

// The destructuring-assignment side of the same code path still binds.
function listAssignBindsLvals($o) {
  list($o->a, $o->b) = array(taint_source(), 1);

  // ruleid: taint-switch-case-php
  sink($o->a);

  // ok: taint-switch-case-php
  sink($o->b);
}

// A constant labels the case (DVWA's `case MYSQL:` / `case SQLITE:`):
// compared, not bound, so the second branch's query keeps its taint.
function bareNameCaseKeepsLaterCases($db) {
  $id = taint_source();
  switch ($db) {
    case MYSQL:
      $query = "SELECT a FROM t WHERE id = '$id'";
      break;
    case SQLITE:
      $query = "SELECT a FROM t WHERE id = '$id'";
      // ruleid: taint-switch-case-php
      sink($query);
      break;
  }
}

function bareNameCaseIsNotABindingTarget() {
  $y = taint_source();
  switch ($y) {
    case RED:
      break;
  }
  // ok: taint-switch-case-php
  sink(RED);
}
