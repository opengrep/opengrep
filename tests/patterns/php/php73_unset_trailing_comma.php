<?php

// unset() may end with a comma

//MATCH:
unset($a, $b,);

//MATCH:
unset($a, $b);

//MATCH:
unset(
    $a,
    $b,
);

//OK: one variable only
unset($a,);

//OK: three variables
unset($a, $b, $c,);
