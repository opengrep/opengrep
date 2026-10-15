<?php

// unlike other constants, 'null' is case-insensitive

//MATCH:
$a = $x === null;

//MATCH:
$b = $x === NULL;

//MATCH:
$c = $x === Null;

//MATCH:
$d = $x === \NULL;

//OK:
$e = $x === false;
