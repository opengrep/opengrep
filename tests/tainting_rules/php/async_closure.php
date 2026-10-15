<?php

// taint reaches a sink inside a closure handed to amphp's async()

use function Amp\async;

function run() {
    $cmd = $_GET['cmd'];

    //ruleid: test-async-closure
    $a = async(fn () => exec($cmd));

    //ruleid: test-async-closure
    $b = async(function () use ($cmd) { exec($cmd); });

    //OK: test-async-closure
    $c = async(fn () => exec('ls'));
}
