<?php

// array unpacking must reach the generic AST as a spread, so a rule can match
// it and bind what is being unpacked. '...' on its own keeps meaning "any
// elements", so the two must not be confused.

//MATCH:
$a = [...$x];

//OK: more than the single unpacked element
$b = [...$x, 4];

//OK: no unpacking at all
$c = [1, 2, 3];

//OK: empty
$d = [];
