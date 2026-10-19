<?php

//ERROR:
$a = function () use ($token) { sink($token); };

//ERROR:
$b = function () use ($other) { sink($other); };

//OK:
$c = function () use ($token, $other) { sink($token); };

//OK:
$d = function () use (&$token) { sink($token); };

//OK:
$e = function () { sink($token); };
