<?php

// an index used in a constant expression must reach the generic AST as the
// access it is, so a rule can find it

//MATCH:
const A = "BAR"[0];

//OK: a different index
const B = "BAR"[1];

//OK: a different subject
const C = "FOO"[0];

//OK: no index at all
const D = "BAR";
