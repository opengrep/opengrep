<?php
// '$f = f(...)' holds the function f, so passing $f calls f

function tainted_callback($x) {
    // ruleid: test-fcc-variable-callback
    sink($x);
}

function clean_callback($x) {
    // ok: test-fcc-variable-callback
    sink("constant");
}

function test_tainted() {
    $f = tainted_callback(...);
    array_map($f, [source()]);
}

function test_clean() {
    $g = clean_callback(...);
    array_map($g, [source()]);
}
