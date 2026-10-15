<?php

// namespace and class names are case-insensitive, also in a qualified name

//MATCH:
$a = new \App\Foo();

//MATCH:
$b = new \APP\FOO(1);

//OK: another class
$c = new \App\Bar();

//OK: variables are case-sensitive
$d = $Foo;
