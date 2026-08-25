<?php

// PHP 7.3: list() entries may be assigned by reference.

list(&$a, &$b) = $arr;
list(&$one, $two) = $arr;
list($one, &$two) = $arr;
list(&$a, list(&$b)) = $arr;
list(&$a[$i]) = $v;
list(&$obj->prop) = $v;
list(, &$b) = $arr;

foreach ($rows as list(&$x, $y)) {
}

// the short syntax already allowed it

[&$a, &$b] = $arr;
[$a, &$b] = $arr;

// plain destructuring must keep working

list($a, $b) = $arr;
list($a, list($b, $c)) = $arr;
[$a, $b] = $arr;
