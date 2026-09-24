<?php
function by_value() {
  $x = 0;
  // ok: test-capture-modes-php
  $l = function () use ($x) { sink($x); };
  $x = source();
  $l();
}

function by_reference() {
  $x = 0;
  // ruleid: test-capture-modes-php
  $l = function () use (&$x) { sink($x); };
  $x = source();
  $l();
}

function arrow_function() {
  $x = 0;
  // ok: test-capture-modes-php
  $l = fn() => sink($x);
  $x = source();
  $l();
}

function tainted_before_creation() {
  $x = source();
  // ruleid: test-capture-modes-php
  $l = function () use ($x) { sink($x); };
  $l();
}
