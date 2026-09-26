<?php
// ERROR:
$f = function() use (&$x) {
    return $x;
};
$g = function() use ($y) {
    return $y;
};
$h = function() {
    return 1;
};
