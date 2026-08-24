<?php

// PHP 8.5: a constant expression may be a closure or a first-class callable.
// PHP requires the closure to be static and forbids 'use', which the parser
// does not enforce.

const F = static function () { return 1; };
const G = static function (int $x): int { return $x * 2; };

const I = strlen(...);
const J = Foo::bar(...);
const K = \App\helper(...);

class C
{
    const L = static function () { return 1; };
    const M = self::make(...);
    public const N = strlen(...);

    public $cb = static function () { return 1; };
    public static $handler = strlen(...);

    public function m(
        $a = static function () { return 1; },
        $b = strlen(...),
    ) {}
}

function f(
    $a = static function () { return 1; },
    $b = Foo::bar(...),
) {}

#[Attr(static function () { return 1; })]
#[Other(strlen(...))]
function g() {}

function h()
{
    static $memo = static function () { return 1; };
}

// constant expressions that already worked must keep working

const P = 1 + 2;
const Q = [1, 2, 3];
const R = self::class;
const S = PHP_EOL;
const T = 1 > 2 ? "a" : "b";
