<?php

// a closure used as a constant expression must reach the generic AST as a
// lambda, so a rule can find constants defined that way

//MATCH:
const F = static function () { return 1; };

//MATCH:
const G = static function () { return 2; };

//OK: an ordinary constant
const H = 1;

//OK: an array, not a closure
const I = [1, 2];
