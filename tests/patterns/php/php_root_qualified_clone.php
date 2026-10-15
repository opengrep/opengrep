<?php

// '\clone' is the same function as 'clone', which no namespace can shadow,
// since PHP forbids declaring a function called clone

//MATCH:
$c = clone($b);

//MATCH:
$d = \clone($b);

//MATCH:
$e = clone $b;

//OK:
$f = $b;
