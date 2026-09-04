<?php

// a property read in a constant expression is matched like any other

enum Suit: string
{
    case Hearts = 'h';
    case Spades = 's';
}

//ERROR:
const A = Suit::Hearts->value;

//ERROR:
class K { const B = Suit::Hearts->value; }

// OK: a different case
const C = Suit::Spades->value;

// OK: a different property
const D = Suit::Hearts->name;
