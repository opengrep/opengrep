<?php

//ERROR:
$a = function () use ($token) { sink($token); };

//OK:
$b = function () use ($token, $other) { sink($token); };

//OK:
$c = function () use ($other, $token) { sink($token); };

//OK:
$d = function () use (&$token) { sink($token); };

//OK:
$e = function () use ($other) { sink($token); };
