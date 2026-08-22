<?php

// PHP 8.5: the '(void)' cast marks that discarding a value is intentional.

(void) foo();
(void) $obj->method();
(void) Foo::bar();
( void ) foo();
(void) $x;

$a = (void) foo();
$b = [(void) foo()];
$c = (void) foo() + 1;

function f(): void
{
    (void) g();
    return;
}

// the other casts and the 'void' return type must keep working

$d = (int) "1";
$e = (float) "1.5";
$f = (bool) 1;
$g = (string) 1;
$h = (array) $x;
$i = (object) $x;

// 'void' as an ordinary identifier is unaffected

$void = 1;
$k = ($void);
