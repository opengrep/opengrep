<?php

//MATCH:
$a = clone $b;

//MATCH:
$a = clone($b);

//OK: an ordinary function call
$a = __builtin__clone($b);
