<?php
// Comprehensive HOF test for PHP: Custom and built-in higher-order functions
// All of these should detect taint flow from source() to sink()

// ===== Custom HOF Functions =====

function customMap($arr, $callback) {
    $result = [];
    foreach ($arr as $item) {
        $result[] = $callback($item);
    }
    return $result;
}

function customForEach($arr, $callback) {
    foreach ($arr as $item) {
        $callback($item);
    }
}

function directCall($callback) {
    $callback(source());
}

// ===== Test Cases =====

function test_custom_map() {
    $arr = [source()];
    customMap($arr, function($x) {
        // ruleid: test-hof-taint
        sink($x);
        return $x;
    });
}

function test_custom_foreach() {
    $arr = [source()];
    customForEach($arr, function($x) {
        // ruleid: test-hof-taint
        sink($x);
    });
}

function test_direct_call() {
    directCall(function($x) {
        // ruleid: test-hof-taint
        sink($x);
    });
}

// ===== Built-in functions =====

function test_builtin_array_map() {
    $arr = [source()];
    array_map(function($x) {
        // ruleid: test-hof-taint
        sink($x);
        return $x;
    }, $arr);
}

function test_builtin_array_filter() {
    $arr = [source()];
    array_filter($arr, function($x) {
        // ruleid: test-hof-taint
        sink($x);
        return true;
    });
}

function test_builtin_array_walk() {
    $arr = [source()];
    array_walk($arr, function($x) {
        // ruleid: test-hof-taint
        sink($x);
    });
}

// ===== Complex Example =====

function getHistory($name, $owner) {
    $result = source();
    return $result;
}

function test_original_example() {
    $history = getHistory("name", "owner");
    customForEach([$history], function($node) {
        $changes = $node;
        // ruleid: test-hof-taint
        sink($changes);
    });
}

// Stub functions
function source() {
    return "tainted";
}

function sink($s) {
}

// ===== Top-level HOF Tests =====
// These test HOF callback detection at script level (outside any function)

// Top-level lambda callback
// ruleid: test-hof-taint
$toplevelSink = function($x) { sink($x); };
$toplevelSink(source());

?>
