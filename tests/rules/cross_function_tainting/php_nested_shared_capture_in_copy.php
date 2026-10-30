<?php
// A closure that shares a variable of a closure that copied it writes the
// copy, not the variable the copy was made from.
function main() {
  $x = "c";
  $a = function () use ($x) {
    $b = function () use (&$x) { $x = source(); };
    $b();
  };
  $a();
  // ok: php_nested_shared_capture_in_copy
  sink($x);
}
function shared() {
  $x = "c";
  $a = function () use (&$x) {
    $b = function () use (&$x) { $x = source(); };
    $b();
  };
  $a();
  // ruleid: php_nested_shared_capture_in_copy
  sink($x);
}
