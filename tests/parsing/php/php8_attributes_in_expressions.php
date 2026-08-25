<?php

// PHP 8.0 allows attributes in expression position, not only before a
// declaration: on closures, on arrow functions and on anonymous classes.

$a = #[A1] function () {};
$b = #[A1("t")] function ($p) { return $p; };
$c = #[A1] static function () {};
$d = #[A1, A2] function () {};

$e = #[A1] fn($x) => $x;
$f = #[A1("t")] static fn($x): int => $x;

$g = new #[A1] class {};
$h = new #[A1(7)] class () {};
$i = new #[A1] class extends Base implements Iface {};

// on a closure and on its parameter at once
$j = #[A1] function (#[A2] $p) {};

// nested
$k = #[A1] function () {
    return #[A2] fn($x) => $x;
};

// as a call argument, which is where php-src exercises it
$l = new ReflectionFunction(#[A1('test')] function () {});

// PHP 8.4: attributes on property hooks

class C
{
    #[A1]
    public $plain;

    public string $hooked {
        #[\Deprecated]
        get => $this->hooked;

        #[A1]
        final set => strtolower($value);
    }
}

// the plain forms must keep working

$m = function () {};
$n = fn($x) => $x;
$o = new class {};
