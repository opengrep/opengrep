<?php

// PHP 7.4 numeric literal separators, and the PHP 8.1 explicit octal prefix.
// The separator only ever sits between two digits.

$dec = 1_000;
$dec2 = 1_000_000;

$hex = 0x1_C;
$hex2 = 0XAB_CD;

$bin = 0b1010_1010;
$bin2 = 0B1010;

$oct = 0o17_7;
$oct2 = 0O17;

$legacy_oct = 017;

$float = 1_000.5;
$float2 = 1.5_5;
$float3 = .5_5;
$exp = 1_0e1_0;
$exp2 = 1_0E-1_0;

// a string offset may carry them too

$s = "$arr[1_0]";

// and the plain forms must keep working

$plain = 1000;
$plain_hex = 0X1A;
$plain_float = 1.5;
$plain_exp = 1e10;

// '_' elsewhere is still an identifier, not part of a number

$_ = 1;
$a_1 = 2;
const _FOO = 3;
