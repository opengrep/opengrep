<?php

// a sink written without a leading '\' also catches the call written with
// one, the style php-cs-fixer's native_function_invocation enforces

namespace App;

function run() {
    $cmd = $_GET['cmd'];

    //ruleid: test-root-qualified-sink
    \exec($cmd);

    //ruleid: test-root-qualified-sink
    exec($cmd);

    //OK: test-root-qualified-sink
    \Other\exec($cmd);
}
