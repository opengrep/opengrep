<?php

function test_custom_map() {
    $arr = [source()];
    customMap($arr, function($x) {
        // ruleid: test-hof-taint
        sink($x);
        return $x;
    });
}
