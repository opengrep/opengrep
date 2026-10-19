<?php

//ERROR:
$a = function () use ($x, $y) { sink($x, $y); };

//ERROR:
$b = function () use ($y, $x) { sink($x, $y); };

//OK:
$c = function () use ($x) { sink($x); };
