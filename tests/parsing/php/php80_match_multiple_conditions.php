<?php

// A match arm may list several conditions separated by commas.

$a = match ($x) {
    1, 2 => "a",
    default => "b",
};

$b = match ($x) {
    1, 2, 3, 4 => "many",
    5 => "one",
    default => "none",
};

$c = match ($suit) {
    Suit::Hearts, Suit::Diamonds => "Red",
    Suit::Clubs, Suit::Spades => "Black",
};

$d = match (true) {
    $n < 0, $n === 0 => "non-positive",
    default => "positive",
};

$e = match ($x) {
    1, 2 => match ($y) {
        3, 4 => "inner",
        default => "other",
    },
    default => "outer",
};

// single-condition arms and no-default arms must keep working

$f = match ($x) {
    1 => "a",
    default => "b",
};

$g = match ($x) {
    1 => "a",
};

$h = match ($x) {
    1, 2 => "a"
};
