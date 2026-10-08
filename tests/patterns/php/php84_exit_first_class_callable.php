<?php

// PHP 8.4: 'exit(...)' is a first-class callable, which does not exit

//MATCH:
exit(1);

//MATCH:
exit();

//MATCH:
die("bye");

//MATCH: piping into it calls it
$code |> exit(...);

//OK: a first-class callable
$f = exit(...);

//OK:
$g = die(...);

//OK: passed as a value
array_map(exit(...), $codes);
