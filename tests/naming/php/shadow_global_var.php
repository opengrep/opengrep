<?php

$counter = seed();

function shadows() {
  // A function body does not see the top-level $counter: the assignment
  // declares a function-local.
  $counter = make();
  sink($counter);
}

function uses_global() {
  // The `global` directive rebinds the top-level $counter; the assignment
  // must NOT declare a local.
  global $counter;
  $counter = make();
  sink($counter);
}
