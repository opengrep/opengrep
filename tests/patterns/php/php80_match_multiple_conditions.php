<?php

// each condition of a match arm must reach the generic AST separately, so a
// rule can ask for an arm listing particular conditions

//MATCH:
$a = match ($x) {
    1, 2 => "hit",
    default => "b",
};

//OK: only one of the two conditions
$b = match ($x) {
    1 => "hit",
    default => "b",
};

//OK: different conditions
$c = match ($x) {
    3, 4 => "other",
    default => "b",
};

//OK: the conditions in the other order
$d = match ($x) {
    2, 1 => "reversed",
    default => "b",
};
