<?php

// PHP 8.5: clone is a function, so beside the prefix form it can be called
// root-qualified, with any arguments, or taken as a first-class callable

//MATCH:
$a = clone $x;

//MATCH:
$b = \clone($x);

//MATCH:
$c = clone($x, $props);

//MATCH:
$d = clone(object: $x);

//MATCH:
$e = clone(...$args);

//MATCH:
$f = clone($x, $props, $extra, );

//MATCH:
$g = \clone($x)->m();

//OK: a first-class callable, which does not clone
$h = clone(...);

//OK: passed as a value
$i = array_map(clone(...), $xs);
