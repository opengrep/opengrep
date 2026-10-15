<?php

// a first-class callable is not a call

//OK:
$g = strlen(...);

//OK:
$f = exit(...);

//OK:
$m = $obj->method(...);

//MATCH:
$h = strlen($s);
