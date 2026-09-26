<?php
$f = function() use (&$x) {
    return $x;
};
// ERROR:
$g = function() use ($y) {
    return $y;
};
$h = function() {
    return 1;
};
