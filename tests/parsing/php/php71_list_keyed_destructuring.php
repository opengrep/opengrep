<?php

// PHP 7.1: list() entries may be keyed, like the short syntax already was.

list("k" => $v) = $arr;
list("a" => $x, "b" => $y) = $arr;
list(0 => $a, 1 => $b) = $arr;
list(K => $v) = $arr;
list($i => $v) = $arr;

// the value may be a reference or a nested list

list("k" => &$v) = $arr;
list("k" => list($a, $b)) = $arr;
list("k" => list("j" => $n)) = $arr;
list("k" => &$v, $rest) = $arr;

foreach ($rows as list("k" => $v)) {
}

foreach ($rows as list("k" => &$v)) {
}

// the short syntax must keep working

["k" => $v] = $arr;
["a" => $x, "b" => $y] = $arr;
["k" => [$a, $b]] = $arr;

// and so must plain destructuring

list($a, $b) = $arr;
list($a, list($b, $c)) = $arr;
list(, $b) = $arr;
[$a, $b] = $arr;
