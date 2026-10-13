<?php

// class names are case-insensitive

//MATCH:
$a = new FOO();

//MATCH:
$b = foo::bar();

//MATCH:
$c = $x instanceof fOO;

//MATCH:
$d = FOO::BAR;

//OK: a constant
$e = foo;
