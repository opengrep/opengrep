<?php

// ${f()} reads the variable whose name f() returns, in a string as well as
// outside one, so the pattern ${f()} matches it in both

//MATCH:
$t = ${f()};

//MATCH:
$u = "${f()}";

//MATCH: among other text
$v = "name: ${f()}!";

//MATCH: with spaces inside the braces
$w = "${ f() }";

//OK: the call itself
$x = f();

//OK: the variable $f, not the variable whose name f() returns
$y = "${f}";
