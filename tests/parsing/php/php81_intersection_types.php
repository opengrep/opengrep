<?php

// PHP 8.1 intersection types, written without parentheses.
//
// They are accepted in return and property position. In parameter position a
// '&' cannot be told apart from a by-reference marker until after it has been
// read, so 'f(X&Y $p)' is not supported; supporting it would break the far
// more common 'f(X &$p)'.

function a(): X&Y {}
function b(): X&Y&Z {}
function c(): Countable&ArrayAccess {}
function d(): \App\X&\App\Y {}

interface I
{
    public function e(): X&Y;
}

abstract class A
{
    abstract public function f(): X&Y;
}

class C
{
    public X&Y $p;
    public X&Y&Z $q;
    private A&B $r;
    protected static X&Y $s;

    public function m(): X&Y {}

    public X&Y $hooked {
        get => $this->hooked;
    }
}

// the parenthesized DNF form keeps working, including mixed with '|'

function g(): (B&C)|A {}
function h(): (B&C) {}

class D
{
    public (B&C)|A $p;
}

// by-reference parameters must keep working

function i(X &$p) {}
function j(&$p) {}
function k(array &$rows, int $n) {}

// and so must unions, nullables and bitwise and

function l(): X|Y {}
function m(): ?X {}
$z = $x & $y;
$w = &$v;
