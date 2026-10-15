<?php

// a skipped element keeps the later elements at their index

//MATCH:
[$a, $b] = explode(':', $s);

//OK: three elements
[$a, , $c] = explode(':', $s);

//OK: three elements
list($a, , $c) = explode(':', $s);

//MATCH: a trailing comma is not an element
[$a, $b,] = explode(':', $s);
