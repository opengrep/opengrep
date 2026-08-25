<?php

// each constant of a multi-declarator statement must reach the generic AST as
// its own definition, so a rule finds the second one just like the first

//MATCH:
const A = 1, B = 2;

//MATCH:
const C = 2;

//OK: no declarator with this value
const D = 1, E = 3;
