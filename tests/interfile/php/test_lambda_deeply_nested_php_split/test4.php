<?php

function test4() {
    $x = source();
    $level1 = function($x) {
        $level2 = function($x) {
            $level3 = function($x) {
                // ruleid: test-lambda-deeply-nested-php
                sink($x);
            };
            $level3($x);
        };
        $level2($x);
    };
    $level1($x);
}
