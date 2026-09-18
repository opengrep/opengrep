package main

// Comprehensive HOF test for Go: Custom higher-order functions
// All of these should detect taint flow from source() to sink()


// ===== Custom HOF Functions =====




// ===== Test Cases =====




// ===== Complex Example =====



// Stub functions


// ===== Top-level HOF Tests =====
// These test HOF callback detection at package level


// Package-level lambda callback (like Python's module-level lambda)
// ruleid: test-hof-taint
var toplevelSink = func(x string) { sink(x) }

