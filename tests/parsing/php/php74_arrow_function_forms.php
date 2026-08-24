<?php

// PHP 7.4 arrow functions: they may be static, return by reference, and
// declare a return type.

$a = fn($x) => $x;
$b = static fn($x) => $x;
$c = fn($x): int => $x;
$d = static fn($x): int => $x;
$e = static fn($x): ?int => $x;
$f = fn&($x) => $x;
$g = fn&($x): array => $x;
$h = fn() => 1;
$i = static fn() => 1;
$j = fn(...$xs) => $xs;
$k = fn(int $x, string $y = "s"): bool => true;
$l = fn($x): static => $x;

// nesting

$m = fn($x) => fn($y) => $x + $y;
$n = static fn($x) => static fn($y): int => $x + $y;

// the long form must keep working

$o = function ($x) { return $x; };
$p = static function ($x) { return $x; };
$q = function ($x) use ($y): int { return $x; };
$r = function &($x) { return $x; };
