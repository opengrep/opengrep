<?php

// a member named with a semi-reserved keyword is matched like any other, both
// where it is declared and where it is used

enum Suit: string
{
    case Case = 'c';
    case Hearts = 'h';
}

//ERROR:
$a = Suit::Case;

//ERROR:
$b = C::Case;

// OK: a different case of the same enum
$c = Suit::Hearts;

// OK: '::class' is its own construct, not a member named 'class'
$d = Suit::class;

// OK: a static property, not a constant
$e = Suit::$Case;
