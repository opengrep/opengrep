<?php

// '??=' (PHP 7.4) and '**=' (PHP 5.6) were the two compound assignment
// operators the parser did not know about.

$a ??= 456;
$ary["foo"] ??= "bar";
$ary[id($foo)] ??= do_throw("ex1");
$obj->prop ??= 1;
$a ??= $b ??= $c;

$x **= 2;
$this->a **= $this->b;
$arr[0] **= 3;

// the operators they are built from must keep working

$n = $b ?? $c;
$m = $b ** $c;

// and so must the compound assignments that already worked

$a += 1;
$a -= 1;
$a *= 2;
$a /= 2;
$a %= 3;
$a .= "x";
$a &= 1;
$a |= 1;
$a ^= 1;
$a <<= 2;
$a >>= 2;
$a = 1;
