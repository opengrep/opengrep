<?php

// PHP 8.5 pipe operator: '$x |> f(...)' passes $x as the only argument to the
// callable on the right.

// --- shapes of the right-hand side ---------------------------------------

$a = "hello" |> strtoupper(...);
$b = $x |> $callable;
$c = $x |> fn($v) => $v + 1;
$d = $x |> function ($v) { return $v; };
$e = $x |> $obj->method(...);
$f = $x |> Foo::bar(...);
$g = $x |> Foo::class;
$h = $x |> ($factory->make())(...);
$i = $x |> \App\helper(...);

// --- chaining (left-associative) -----------------------------------------

$j = "hello" |> strtoupper(...) |> strrev(...);
$k = $x |> a(...) |> b(...) |> c(...);

// --- precedence -----------------------------------------------------------
// arithmetic binds tighter, comparison and ?? bind looser

$l = 5 + 2 |> f(...);
$m = "beep" |> strlen(...) == 4;
$n = $id |> get_username(...) ?? "default";
$o = $x . $y |> f(...);
$p = -$x |> f(...);
$q = $x |> f(...) ? 1 : 2;

// --- interaction with other expressions ------------------------------------

$r = [1, 2, 3] |> array_sum(...);
$s = ($x |> f(...)) |> g(...);
$t = f($x |> g(...));
$u = $arr[$i] |> f(...);
$v = $x |> f(...) + 1;

// '|' and '>' must still lex separately
$w = $x | $y;
$y2 = $x > $y;
$z = $x | $y > 1;
