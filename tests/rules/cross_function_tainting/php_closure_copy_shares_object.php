<?php
// A closure that copies a captured variable shares the object it holds,
// and its $this is the object of the method defining it.
function set_field() {
  $obj = new stdClass();
  array_map(function ($i) use ($obj) { $obj->v = source(); }, [1]);
  // ruleid: php_closure_copy_shares_object
  sink($obj->v);
}
function reassign() {
  $obj = "safe";
  $f = function () use ($obj) { $obj = source(); };
  $f();
  // ok: php_closure_copy_shares_object
  sink($obj);
}
class K {
  public $x;
  function later() {
    $this->x = source();
    $f = function () { return $this->x; };
    $this->x = "c";
    // ok: php_closure_copy_shares_object
    sink($f());
  }
}
