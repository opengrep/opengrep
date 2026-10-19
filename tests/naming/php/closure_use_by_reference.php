<?php

function creates_the_variable() {
  // The closure's use (&$u) creates $u in this function, where nothing
  // assigned it before: the write in the closure is read after the call.
  $g = function () use (&$u) {
    $u = make();
  };
  $g();
  sink($u);
}
