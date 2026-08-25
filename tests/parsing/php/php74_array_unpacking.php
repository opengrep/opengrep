<?php

// PHP 7.4: array unpacking inside an array literal. String keys are allowed
// since PHP 8.1.

$a = [...$x];
$b = [...$x, ...$y];
$c = [1, ...$x, 2];
$d = [...$x, "p" => 1];
$e = ["p" => 1, ...$x];
$f = [...foo()];
$g = [...$obj->items];
$h = [...[1, 2], 3];
$i = [...$x,];

// the long array syntax too

$j = array(...$x);
$k = array(1, ...$x, 2);

// unpacking in call arguments still works

foo(...$args);
foo(1, ...$args);

// ordinary arrays are unaffected

$l = [];
$m = [1, 2, 3];
$n = ["p" => 1, "q" => 2];
$o = [&$ref];
