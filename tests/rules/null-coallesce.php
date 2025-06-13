<?php

function f($filename) {
    // ruleid: test
    "convert $filename";
}
function f1($filename) {
    return shell_exec($filename)
            ?? 'Failed to get filename';
}

