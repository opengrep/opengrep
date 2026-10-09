<?php

// PHP 8.5: clone is a function, so it takes named arguments

//MATCH:
$a = clone(object: $x, withProperties: ['p' => 1]);

//MATCH: root-qualified
$b = \clone(object: $x, withProperties: $props);

//MATCH: a trailing comma
$c = clone(
    object: $x,
    withProperties: $props,
);

//OK: positional arguments
$d = clone($x, $props);

//OK: only the object
$e = clone(object: $x);
