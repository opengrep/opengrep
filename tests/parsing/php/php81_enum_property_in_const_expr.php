<?php

// A property may be read off a class constant in a constant expression, which
// is how an enum case's name or value is used there.

enum Suit: string
{
    case Hearts = 'H';
    case Spades = 'S';
}

const N = Suit::Hearts->name;
const V = Suit::Hearts->value;
const W = Suit::Hearts?->value;

#[Attr(Suit::Hearts->value)]
function attributed() {}

function withDefault($v = Suit::Hearts->value) {}

class C
{
    public $prop = Suit::Hearts->value;

    const K = Suit::Hearts->value;

    public function m($v = Suit::Spades->name) {}
}

enum Other: string
{
    case A = 'a';

    const FIRST = self::A->value;
}

// plain class constant fetches must keep working

const B = Foo::BAR;
const D = Foo::class;
const E = self::BAR;

// and so must property access in expression position

$a = Suit::Hearts->value;
$b = $obj->prop;
$c = $obj?->prop;
