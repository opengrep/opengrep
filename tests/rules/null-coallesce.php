<?php

function f($filename) {
    // ruleid: test
    "convert $filename";
}
function f1($filename) {
    // ruleid: test
    return shell_exec($filename)
            ?? throw new RuntimeException('Failed to get filename');
}

