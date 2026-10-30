<?php
// A property whose name is computed is a property of the object, which the
// caller shares; an element at a computed index is one of the array copy.

function fill_property($o, $name) {
  $o->$name = source();
}

function fill_element($a, $key) {
  $a[$key] = source();
}

function test_property() {
  $obj = new stdClass();
  $obj->k = "safe";
  fill_property($obj, "k");
  // ruleid: test-dynamic-property-parameter
  sink($obj->k);
}

function test_element() {
  $arr = array("k" => "safe");
  fill_element($arr, "k");
  // ok: test-dynamic-property-parameter
  sink($arr['k']);
}
