<?php

// short list destructuring can skip elements

//MATCH:
[, $b] = explode(':', $s);

//MATCH:
[$a, , $c] = explode(':', $s);

//MATCH:
[, [, $d]] = explode(':', $s);

//MATCH:
[, &$e] = explode(':', $s);

//OK: not destructured
$parts = explode(':', $s);

//OK: a different call
[, $b] = str_split($s);
