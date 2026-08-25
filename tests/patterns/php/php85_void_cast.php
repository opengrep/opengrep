<?php

// a '(void)' cast must reach the generic AST as a cast to void, so a rule can
// find the places where discarding a value was deliberate

//MATCH:
(void) foo();

//MATCH:
(void) $obj->method();

//OK: a different cast
(int) foo();

//OK: no cast at all
foo();
