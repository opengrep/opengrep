<?php

function test4_level1($x) {
    $level2 = function($x) {
        $level3 = function($x) {
            // ruleid: test-lambda-deeply-nested-php
            sink($x);
        };
        $level3($x);
    };
    $level2($x);
}
