<?php

// however a heredoc is written, its value is the same: the marker's
// indentation is not part of it, and neither is the newline before the marker

//ERROR:
$indented = <<<TXT
    hello
    TXT;

//ERROR:
$flushLeft = <<<TXT
hello
TXT;

//ERROR:
$tabbed = <<<TXT
	hello
	TXT;

//ERROR:
$nowdoc = <<<'TXT'
    hello
    TXT;

//ERROR: the marker followed by something other than a semicolon
$parenthesised = (<<<TXT
    hello
    TXT);

//ERROR: and the plain string it is equal to
$plain = "hello";

// OK: a different value
$other = <<<TXT
    goodbye
    TXT;

// OK: the indentation of this line is deeper than the marker's, so what is
// left of it is part of the value
$deeper = <<<TXT
      hello
    TXT;
