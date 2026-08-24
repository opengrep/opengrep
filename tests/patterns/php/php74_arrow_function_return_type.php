<?php

// an arrow function's return type must reach the generic AST, so a rule can
// tell one return type from another

//MATCH:
$a = fn($x): int => $x;

//MATCH: 'static' is not represented on a lambda, so this matches too
$b = static fn($x): int => $x;

//OK: a different return type
$c = fn($x): string => $x;

//OK: no return type
$d = fn($x) => $x;
