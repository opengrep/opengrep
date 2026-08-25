<?php

// '::' is allowed after a call, so a class constant fetch on the result of a
// call must reach the generic AST

//MATCH:
$a = foo()::MAX;

//OK: different constant
$b = foo()::MIN;

//OK: different callee
$c = bar()::MAX;

//OK: not a call
$d = Foo::MAX;
