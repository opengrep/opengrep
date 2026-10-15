<?php

// cast names are case-insensitive, and a name in parentheses that is not one
// is just a parenthesized expression

//MATCH:
$a = (string) $x;

//MATCH:
$b = (String) Store::DISTRO_STORE_ID;

//MATCH: with spaces
$c = ( STRING ) $x;

//MATCH: 'binary' is a string cast
$d = (binary) $x;

//OK: a different cast
$e = (Int) $x;

//OK: a constant in parentheses
$f = (STRING_CONST);
