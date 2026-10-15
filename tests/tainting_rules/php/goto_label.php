<?php

// a label is a statement of its own, which the flow reaches both by falling
// through and by a goto

function backward($c) {
    $a = 1;
    l:
    //ruleid: test-goto-label
    sink($a);
    $a = source();
    if ($c) goto l;
}

function forward() {
    $b = source();
    goto end;
    $b = 1;
    end:
    //ruleid: test-goto-label
    sink($b);
}

function at_end_of_block() {
    try {
    } finally {
        $d = source();
        goto out;
        out:
    }
    //ruleid: test-goto-label
    sink($d);
}

function overwritten() {
    $e = source();
    l:
    $e = 1;
    //OK: test-goto-label
    sink($e);
}
