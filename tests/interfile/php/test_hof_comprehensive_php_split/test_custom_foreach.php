<?php

function test_custom_foreach() {
    $arr = [source()];
    customForEach($arr, function($x) {
        // ruleid: test-hof-taint
        sink($x);
    });
}
