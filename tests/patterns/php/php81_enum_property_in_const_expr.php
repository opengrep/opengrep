<?php

// reading an enum case's value in a constant expression must reach the
// generic AST as the property access it is, so a rule can find it

//MATCH:
const A = Suit::Hearts->value;

//OK: a different property
const B = Suit::Hearts->name;

//OK: a different case
const C = Suit::Spades->value;

//OK: a plain class constant, no property access
const D = Suit::Hearts;
