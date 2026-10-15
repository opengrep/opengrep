<?php
// '$obj->m(...)' passed to a higher-order function is the method m itself

class Handler {
    function tainted_callback($x) {
        // ruleid: test-fcc-method-callback
        sink($x);
    }

    function clean_callback($x) {
        // ok: test-fcc-method-callback
        sink("constant");
    }
}

function test_tainted() {
    $h = new Handler();
    array_map($h->tainted_callback(...), [source()]);
}

function test_clean() {
    $h = new Handler();
    array_map($h->clean_callback(...), [source()]);
}
