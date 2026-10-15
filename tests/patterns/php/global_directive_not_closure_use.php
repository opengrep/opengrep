<?php

function f() {
  //OK:
  $g = function () use ($x) { sink($x); };
  //ERROR:
  global $x;
}
