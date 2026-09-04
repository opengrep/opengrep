<?php

// a backslash before a line break is a literal backslash, and the break is
// still a line boundary, so the next line is dedented like any other

//ERROR:
$indented = <<<TXT
    a\
    b
    TXT;

//ERROR:
$flushLeft = <<<TXT
a\
b
TXT;

//ERROR: and the plain string they are equal to
$plain = "a\
b";

// OK: no backslash, so a different string
$noBackslash = <<<TXT
    a
    b
    TXT;
