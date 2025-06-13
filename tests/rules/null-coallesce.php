<?php

function f($filename) {
    // ruleid: embed_word
    "convert $filename";
}
function f1($filename) {
    return shell_exec($filename)
            ?? throw new RuntimeException('Failed to get filename');
}

