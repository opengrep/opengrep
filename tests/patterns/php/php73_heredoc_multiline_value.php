<?php

// the newlines inside a heredoc are part of its value, and the marker's
// indentation is not, so all of these are the same two-line string

//ERROR:
$indented = <<<TXT
    one
    two
    TXT;

//ERROR:
$flushLeft = <<<TXT
one
two
TXT;

//ERROR:
$nowdoc = <<<'TXT'
    one
    two
    TXT;

//ERROR: and the plain string they are equal to
$plain = "one
two";

// OK: a blank line between them makes a different string
$spaced = <<<TXT
    one

    two
    TXT;

// OK: joined, as they wrongly used to be
$joined = "onetwo";
