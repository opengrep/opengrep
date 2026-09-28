<?php
// ERROR:
$f = function() use (&$x, $y) {
    return $x;
};
// ERROR:
$g = function() use ($y, &$x) {
    return $x;
};
// ERROR:
$h = function() use (&$x) {
    return $x;
};
$i = function() use ($x, $y) {
    return $x;
};
$j = function() use ($y) {
    return $y;
};
