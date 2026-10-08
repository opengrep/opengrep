<?php

// isset() may end with a comma

//MATCH:
$r = isset($a, $b,);

//MATCH:
$r = isset($a, $b);

//MATCH:
if (isset(
    $a,
    $b,
)) {}

//OK: one variable only
$r = isset($a,);

//OK: three variables
$r = isset($a, $b, $c,);
