<?php

// a trailing comma is allowed in the attribute list and in an attribute's
// arguments, as it is in every other PHP argument list

#[A("many", "arguments",)]
class C
{
    #[B(x: 1, y: 2,)]
    public int $p = 1;

    #[C(1,)]
    public function m(#[D(2,)] int $a) {}
}

#[E, F,]
function f() {}

#[G,]
function g() {}

#[H(),]
function h() {}
