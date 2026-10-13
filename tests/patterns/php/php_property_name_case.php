<?php

// property names are case-sensitive

//MATCH:
echo $o->foo;

//OK:
echo $o->FOO;

//MATCH: the name of a method, unlike that of a property, is case-insensitive
$o->FOO();
