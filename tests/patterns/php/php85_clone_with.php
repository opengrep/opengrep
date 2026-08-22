<?php

// clone-with must reach the generic AST as a two-argument clone, so that it
// stays distinguishable from an ordinary clone

//MATCH:
$a = clone($obj, ["p" => 1]);

//MATCH:
$b = clone($other, $props);

//OK: an ordinary clone
$c = clone $obj;

//OK: an ordinary clone, written with parentheses
$d = clone($obj);
