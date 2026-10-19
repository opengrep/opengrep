<?php

//ERROR:
$a = function () use ($token) { sink($token); };

//ERROR:
$b = function () { sink($token); };
