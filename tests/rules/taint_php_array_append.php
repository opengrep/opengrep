<?php

function append_keeps_taint() {
    $ids = source();
    $ids[] = -5;
    // ruleid: taint-php-array-append
    sink($ids);
}

function reassignment_cleans() {
    $ids = source();
    $ids = [-5];
    // ok: taint-php-array-append
    sink($ids);
}
