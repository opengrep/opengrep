<?php

function test_builtin_array_map() {
    $arr = [source()];
    array_map(function($x) {
        // ruleid: test-hof-taint
        sink($x);
        return $x;
    }, $arr);
}
