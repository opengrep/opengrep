<?php
// A closure returned by the function that makes it holds the values it
// copied when it was created.

function make_tainted() {
  $x = source();
  $f = function () use ($x) { return $x; };
  $x = "safe";
  return $f;
}

function make_safe() {
  $x = "safe";
  $f = function () use ($x) { return $x; };
  $x = source();
  return $f;
}

function use_tainted() {
  $g = make_tainted();
  // ruleid: php_returned_closure_holds_its_copy
  sink($g());
}

function use_safe() {
  $g = make_safe();
  // ok: php_returned_closure_holds_its_copy
  sink($g());
}
