<?php

//ERROR:
$a = function ($x) use ($y) { return $y; };

//ERROR:
$b = function ($x) { return $x; };

//OK:
$c = function ($x) use ($y) { g($y); return $y; };
