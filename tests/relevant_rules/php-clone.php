<?php

$a = clone $x;
$b = \clone($x);
$c = clone(object: $x, withProperties: ['p' => 1]);
