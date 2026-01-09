<?php
// Test that ... in patterns matches any arguments
// This should NOT be confused with PHP 8.1 first-class callables

function test() {
    // ruleid: ellipsis-args
    foo();

    // ruleid: ellipsis-args
    foo(1);

    // ruleid: ellipsis-args
    foo(1, 2, 3);

    // ruleid:ellipsis-args
    foo("test", $var, null);

    // ok: ellipsis-args
    bar();

    // ok: ellipsis-args
    bar(1, 2);

    // ok: ellipsis-args
    // PHP 8.1 first-class callable - not a call with args
    $callable = foo(...);
}
