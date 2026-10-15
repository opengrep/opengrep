<?php
// 'f(...)' passed to a higher-order function is the function f itself

function tainted_callback($x) {
    // ruleid: test-fcc-function-callback
    sink($x);
}

function clean_callback($x) {
    // ok: test-fcc-function-callback
    sink("constant");
}

function test_tainted() {
    array_map(tainted_callback(...), [source()]);
}

function test_clean() {
    array_map(clean_callback(...), [source()]);
}
