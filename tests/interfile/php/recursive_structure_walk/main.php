<?php

require_once 'walk.php';

function run() {
    $w = new Walker();
    $w->walk(source());
}

function run_clean() {
    $w = new Walker();
    $w->walk(new stdClass());
}
