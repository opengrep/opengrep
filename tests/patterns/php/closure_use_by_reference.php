<?php

//ERROR:
$a = function () use (&$token) { sink($token); };

//OK:
$b = function () use ($token) { sink($token); };
