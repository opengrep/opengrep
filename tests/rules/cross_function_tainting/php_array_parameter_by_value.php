<?php
// An array argument is copied into the parameter: the callee's update of the
// copy does not reach the caller. A reference parameter and an object do.

function fill($a) {
  $a['k'] = source();
}

function fill_reference(&$a) {
  $a['k'] = source();
}

function fill_object($o) {
  $o->k = source();
}

function test_by_value() {
  $arr = array();
  fill($arr);
  // ok: test-array-parameter-by-value
  sink($arr['k']);
}

function test_by_reference() {
  $arr = array();
  fill_reference($arr);
  // ruleid: test-array-parameter-by-value
  sink($arr['k']);
}

function test_object() {
  $obj = new stdClass();
  fill_object($obj);
  // ruleid: test-array-parameter-by-value
  sink($obj->k);
}
