<?php


function f($filename) {
    // ruleid: AIK_exec-use
    return shell_exec($filename)
            ?? throw new RuntimeException('Failed to get filename');
}

