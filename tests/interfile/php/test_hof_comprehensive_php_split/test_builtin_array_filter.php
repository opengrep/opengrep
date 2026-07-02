<?php

function test_builtin_array_filter() {
    $arr = [source()];
    array_filter($arr, function($x) {
        // ruleid: test-hof-taint
        sink($x);
        return true;
    });
}
