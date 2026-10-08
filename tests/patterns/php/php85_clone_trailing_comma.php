<?php

// a comma after the object alone gives an ordinary clone

//MATCH:
$a = clone($obj, );

//MATCH:
$b = clone($obj);

//MATCH:
$c = clone $obj;

//OK: clone-with
$d = clone($obj, $props, );
