<?php

// an enum case, a method and a class constant may be named with any of the
// keywords PHP calls semi-reserved, as a named-argument label already could

enum Suit: string
{
    case Case = 'case';
    case List = 'list';
    case Default = 'default';
    case Hearts = 'H';
}

class C
{
    const Case = 1;
    const List = 2;

    public function case() {}

    public function print() {}

    public static function default() {}
}

// and referring to them, which is the '::' side of the same rule

$a = Suit::Case;
$b = C::Case;
$c = C::default();
$d = $obj->case();

// the other three things that may follow '::' are unaffected

$e = C::class;
$f = C::$prop;
$g = C::{$name};
