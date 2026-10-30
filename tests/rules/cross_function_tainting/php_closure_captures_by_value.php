<?php
// A closure copies a variable of its use list when it is created, unless the
// variable is declared by reference, which shares it.

function write_copy() {
  $x = "";
  $g = function () use ($x) { $x = source(); };
  $g();
  // ok: php_closure_captures_by_value
  sink($x);
}

function write_shared() {
  $x = "";
  $g = function () use (&$x) { $x = source(); };
  $g();
  // ruleid: php_closure_captures_by_value
  sink($x);
}

function read_copy_of_safe() {
  $x = "safe";
  // ok: php_closure_captures_by_value
  $f = function () use ($x) { sink($x); };
  $x = source();
  $f();
}

function read_copy_of_tainted() {
  $x = source();
  // ruleid: php_closure_captures_by_value
  $f = function () use ($x) { sink($x); };
  $x = "safe";
  $f();
}

function read_shared() {
  $x = "safe";
  // ruleid: php_closure_captures_by_value
  $f = function () use (&$x) { sink($x); };
  $x = source();
  $f();
}
