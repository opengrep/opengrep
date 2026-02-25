<?php

// Test: Deeply nested lambdas (3 levels)
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

// Test: Deeply nested lambdas split across functions
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

function test4_caller() {
    $x = source();
    test4_level1($x);
}

function source() { return "tainted"; }
function sink($x) {}
