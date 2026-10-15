<?php

// '\null' is null, which no namespace can shadow

//MATCH:
$a = $x === null;

//MATCH:
$b = $x === NULL;

//MATCH:
$c = $x === \null;

//OK:
$d = $x === false;
