<?php

function test_builtin_array_walk() {
    $arr = [source()];
    array_walk($arr, function($x) {
        // ruleid: test-hof-taint
        sink($x);
    });
}
