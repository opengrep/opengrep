<?php

// PHP 8.3: a property may be read in a constant expression. The operand is any
// constant expression, not only a class constant; PHP rejects the ones that
// are not objects when the constant is evaluated.

enum Suit: string
{
    case Hearts = 'h';
}

const V = Suit::Hearts->value;
const N = Suit::Hearts->name;
const P = (new A)->prop;
const Q = (42)->prop;
const R = (null)?->test;
const S = __file__->foo;
const T = __file__?->foo;
const U = (new A)->{new B};
const W = Suit::Hearts->a->b;

class C
{
    const K = self::X->value;

    public $prop = Suit::Hearts->value;

    public function m($v = Suit::Hearts->value) {}
}

#[Attr(Suit::Hearts->value)]
function f() {}
