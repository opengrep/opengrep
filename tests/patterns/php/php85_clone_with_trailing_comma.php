<?php

// clone() may end with a comma

//MATCH:
$a = clone($obj, ["p" => 1], );

//MATCH:
$b = clone(
    $obj,
    $props,
);

//OK: a comma after the object alone gives an ordinary clone
$c = clone($obj, );
