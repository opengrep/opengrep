<?php

$config = source();

function reads_without_global() {
    // A function body does not see the top-level variable.
    // ok: php-function-scope
    sink($config);
}

function reads_with_global() {
    global $config;
    // ruleid: php-function-scope
    sink($config);
}

function reads_with_upper_case_global() {
    GLOBAL $config;
    // ruleid: php-function-scope
    sink($config);
}

// ruleid: php-function-scope
$arrow = fn() => sink($config);

$closure = function () use ($config) {
    // ruleid: php-function-scope
    sink($config);
};

$plain = function () {
    // ok: php-function-scope
    sink($config);
};
