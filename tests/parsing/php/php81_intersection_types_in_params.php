<?php

// an intersection type may be a parameter's type. The '&' is told apart from
// the one introducing a by-reference parameter by what follows it.

function a(X&Y $p) {}

function b(X&Y&Z $p) {}

function c(X&Y $p = null) {}

function d(\A\B & \C\D $p) {}

class K
{
    public function __construct(public X&Y $promoted) {}

    public function m(Countable&ArrayAccess $c): X&Y {}
}

// by-reference parameters are unchanged

function e(X &$p) {}

function f(&$p) {}

function g(&...$p) {}

function h(X &...$p) {}

// a reference target need not start with '$', so these must keep working

class M
{
    public static $s;

    public function m()
    {
        foreach ([1] as &self::$s) {}
        foreach ([1] as &M::$s) {}
        foreach ([1] as &static::$s) {}
        list(&M::$s) = [1];
        $a = [&M::$s];
        $b = ['k' => &M::$s];
        n(&M::$s);
        $c = &self::$s;
        return [$a, $b, $c];
    }
}

// and so is every other use of '&'

function i()
{
    $a = $b & $c;
    $d = $b & MASK;
    $e = &$b;
    $f = &i();
    foreach ($a as &$v) {}
    $g = function () use (&$v) {};
    list(&$x, &$y) = $a;
    return [$a, $d, $e, $f, $g, $x, $y];
}

function &j() { return $GLOBALS; }

class L
{
    public function &m() {}
}
