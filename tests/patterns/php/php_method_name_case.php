<?php

// method names are case-insensitive

//MATCH:
$o->foo();

//MATCH:
$o->FOO(1);

//MATCH:
$o?->Foo();

//OK: another method
$o->bar();
