<?php

// in a pattern of the clone function, '...' may come before other arguments

//MATCH:
$a = clone($x, $props);

//MATCH: the prefix form, with a single argument
$b = clone $x;

//MATCH:
$c = clone($x, ['p' => 1]);

//OK: no arguments
$d = clone();
