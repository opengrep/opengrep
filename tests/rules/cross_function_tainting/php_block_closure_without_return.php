<?php
// A block body returns a value only through a return statement.
function ends_with_new() {
  $f = function ($x) { new Foo($x); };
  // ok: php_block_closure_without_return
  sink($f(source()));
}

function returns_new() {
  $f = function ($x) { return new Foo($x); };
  // ruleid: php_block_closure_without_return
  sink($f(source()));
}
