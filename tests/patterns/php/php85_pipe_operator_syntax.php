<?php

// a rule written with the pipe syntax must match piped calls only, so that a
// piped call stays distinguishable from a direct one

//MATCH:
$b |> sink(...);

//MATCH:
$c |> escape(...) |> sink(...);

//OK: a direct call, not piped
sink($a);

//OK: piped into a different callee
$d |> other(...);
