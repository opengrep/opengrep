<?php

// $_GET reaches $aN only after N + 1 passes of the loop, so the fixpoint
// has to visit each loop node about N + 2 times to find the flow.

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
  // ruleid: taint-loop-visit-cap-raised
  echo $a3;
}

function chain_6($c) {
  $a0 = "";
  $a1 = "";
  $a2 = "";
  $a3 = "";
  $a4 = "";
  $a5 = "";
  $a6 = "";
  while ($c) {
    $a6 = $a5;
    $a5 = $a4;
    $a4 = $a3;
    $a3 = $a2;
    $a2 = $a1;
    $a1 = $a0;
    $a0 = $_GET["x"];
  }
  // ruleid: taint-loop-visit-cap-raised
  echo $a6;
}

// Needs 9 visits per node, one more than the rule allows.
function chain_7($c) {
  $a0 = "";
  $a1 = "";
  $a2 = "";
  $a3 = "";
  $a4 = "";
  $a5 = "";
  $a6 = "";
  $a7 = "";
  while ($c) {
    $a7 = $a6;
    $a6 = $a5;
    $a5 = $a4;
    $a4 = $a3;
    $a3 = $a2;
    $a2 = $a1;
    $a1 = $a0;
    $a0 = $_GET["x"];
  }
  // todoruleid: taint-loop-visit-cap-raised
  echo $a7;
}
