<?php

function f($filename) {
    // ruleid: test
    bad("convert $filename");
}
function f1($filename) {
    // ruleid: test
    return bad($filename)
            + throw new RuntimeException('Failed to do something bad');
}
function f2($filename) {
    // ruleid: test
    return bad($filename)
            ?? throw new RuntimeException('Failed to do something bad');
}

