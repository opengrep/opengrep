<?php

// the conditions of a match arm, and 'default', may end with a comma

//MATCH:
$a = match ($x) {
    1, 2, => "hit",
    default => "b",
};

//MATCH:
$b = match ($x) {
    1,
    2,
        => "hit",
    default,
        => "b",
};

//OK: only one of the two conditions
$c = match ($x) {
    1, => "hit",
    default, => "b",
};

//OK: different conditions
$d = match ($x) {
    3, 4, => "other",
};
