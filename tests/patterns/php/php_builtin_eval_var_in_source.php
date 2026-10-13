<?php

//MATCH:
$a = ${f()};

//MATCH:
$b = "${f()}";

//OK: an ordinary function call
$c = __builtin__eval_var(f());
