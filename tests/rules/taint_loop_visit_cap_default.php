<?php

// $_GET reaches $aN only after N + 1 passes of the loop, so the fixpoint
// has to visit each loop node about N + 2 times to find the flow.

function chain_2($c) {
  $a0 = "";
  $a1 = "";
  $a2 = "";
  while ($c) {
    $a2 = $a1;
    $a1 = $a0;
    $a0 = $_GET["x"];
  }
  // ruleid: taint-loop-visit-cap-default
  echo $a2;
}

// The default limit of 4 visits per node drops this flow.
function chain_3($c) {
  $a0 = "";
  $a1 = "";
  $a2 = "";
  $a3 = "";
  while ($c) {
    $a3 = $a2;
    $a2 = $a1;
    $a1 = $a0;
    $a0 = $_GET["x"];
  }
  // todoruleid: taint-loop-visit-cap-default
  echo $a3;
}
