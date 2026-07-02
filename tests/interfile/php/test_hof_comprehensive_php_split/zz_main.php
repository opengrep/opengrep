<?php

// Comprehensive HOF test for PHP: Custom and built-in higher-order functions
// All of these should detect taint flow from source() to sink()

// ===== Custom HOF Functions =====




// ===== Test Cases =====




// ===== Built-in functions =====




// ===== Complex Example =====



// Stub functions


// ===== Top-level HOF Tests =====
// These test HOF callback detection at script level (outside any function)

// Top-level lambda callback
// ruleid: test-hof-taint
$toplevelSink = function($x) { sink($x); };
$toplevelSink(source());

?>
