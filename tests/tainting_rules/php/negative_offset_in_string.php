<?php

// PHP 7.1: reading an array at a negative offset inside a string carries the
// taint of the array

function test() {
    $a = source();
    //ruleid: test-negative-offset-in-string
    sink("value: $a[-1]");

    $b = safe();
    //OK: test-negative-offset-in-string
    sink("value: $b[-1]");
}
