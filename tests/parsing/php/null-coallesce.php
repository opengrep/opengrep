<?php

function f($filename) {
    return shell_exec($filename)
            ?? throw new RuntimeException('Failed to get filename');
}

