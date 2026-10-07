<?php
const CMD = 7;
function test($mode) {
  // ruleid: switch-case-constant-propagation-php
  exec(CMD);
  switch ($mode) {
    case CMD:
      // ruleid: switch-case-constant-propagation-php
      exec(CMD);
      break;
  }
  // ruleid: switch-case-constant-propagation-php
  exec(CMD);
}

function matchConstant($mode) {
  $q = match ($mode) {
    // ruleid: switch-case-constant-propagation-php
    CMD => exec(CMD),
    default => 0,
  };
}

// Qualified and bare case labels read values, without binding the
// scrutinee to the constant.
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

// Destructuring assignments still bind their targets.
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

function matchArmsKeepLaterValues($db) {
  $q = match ($db) {
    MYSQL => "safe",
    // ruleid: taint-switch-case-php
    SQLITE => sink(taint_source()),
    default => "safe",
  };
}

function matchLabelsAreNotBound() {
  $y = taint_source();
  $q = match ($y) {
    RED => "safe",
    default => "safe",
  };
  // ok: taint-switch-case-php
  sink(RED);
  // ok: taint-switch-case-php
  sink($q);
}

function matchResultKeepsTaint($mode) {
  $value = match ($mode) {
    MYSQL => "safe",
    SQLITE => taint_source(),
    default => "safe",
  };
  // ruleid: taint-switch-case-php
  sink($value);
}

function numericMatchResultKeepsTaint($mode) {
  $value = match ($mode) {
    1 => "safe",
    2 => taint_source(),
    default => "safe",
  };
  // ruleid: taint-switch-case-php
  sink($value);
}

function matchDefaultCanComeFirst($mode) {
  $value = match ($mode) {
    default => "safe",
    SQLITE => taint_source(),
  };
  // ruleid: taint-switch-case-php
  sink($value);
}

function ordinarySwitchStillFallsThrough($mode) {
  $value = "safe";
  switch ($mode) {
    case MYSQL:
      $value = taint_source();
    case SQLITE:
      // ruleid: taint-switch-case-php
      sink($value);
      break;
  }
}
