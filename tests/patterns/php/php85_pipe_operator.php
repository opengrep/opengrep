<?php

// a piped call is the call it stands for, so a rule written against the plain
// call must also match the piped form

//MATCH:
sink($a);

//MATCH:
$b |> sink(...);

//MATCH:
$c |> escape(...) |> sink(...);

//OK: a different callee
$d |> other(...);

//OK: piped into something else
$e |> sink2(...);
