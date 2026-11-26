<?php

function append_keeps_taint() {
    $ids = source();
    $ids[] = -5;
    // ruleid: taint-php-array-append
    sink($ids);
}

function nested_append_keeps_taint() {
    $rows = source();
    $rows[]['id'] = -5;
    // ruleid: taint-php-array-append
    sink($rows);
}

function append_of_tainted_value() {
    $ids = [];
    $ids[] = source();
    // ruleid: taint-php-array-append
    sink($ids);
}

function reassignment_cleans() {
    $ids = source();
    $ids = [-5];
    // ok: taint-php-array-append
    sink($ids);
}

function index_write_cleans($k) {
    $ids = source();
    $ids[$k] = -5;
    // ok: taint-php-array-append
    sink($ids);
}
