<?php
function set(&$x) { $x = source(); }

function caller() {
  $s = "";
  set($s);
  // ruleid: test-param-by-reference-php
  sink($s);
}
