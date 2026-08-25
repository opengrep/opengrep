<?php

// PHP is case-insensitive for the boolean literals.

$a = true;
$b = false;

$c = TRUE;
$d = FALSE;

$e = True;
$f = False;

$g = tRuE;
$h = fAlSe;

if (TRUE) {
}

foo(FALSE);

const K = TRUE;

class C
{
    public $p = FALSE;

    public function m($x = TRUE) {}
}

$arr = [TRUE, FALSE, True, false];
