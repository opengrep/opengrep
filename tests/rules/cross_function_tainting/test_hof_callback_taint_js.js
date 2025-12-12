// Test HOF taint propagation based on callback behavior.
// Tests both callback-only flow and callback + direct flow patterns.

function propagates(x) {
    return x;
}

function sanitizes(x) {
    return 3;
}

// HOF where taint flows only through callback return
function app_callback_only(f, x) {
    return f(x);
}

// HOF where taint flows through callback AND directly via x
function app_with_direct_flow(f, x) {
    return f(x) + x;
}

// === Callback-only HOF tests ===

function test_callback_only_propagating_named() {
    // ruleid: test-hof-callback-taint
    return sink(app_callback_only(propagates, source()));
}

function test_callback_only_propagating_lambda() {
    // ruleid: test-hof-callback-taint
    return sink(app_callback_only(x => x, source()));
}

function test_callback_only_sanitizing_named() {
    // ok: test-hof-callback-taint
    return sink(app_callback_only(sanitizes, source()));
}

function test_callback_only_sanitizing_lambda() {
    // ok: test-hof-callback-taint
    return sink(app_callback_only(x => 3, source()));
}

// === Direct flow HOF tests (taint always flows via + x) ===

function test_direct_flow_propagating_named() {
    // ruleid: test-hof-callback-taint
    return sink(app_with_direct_flow(propagates, source()));
}

function test_direct_flow_propagating_lambda() {
    // ruleid: test-hof-callback-taint
    return sink(app_with_direct_flow(x => x, source()));
}

function test_direct_flow_sanitizing_named() {
    // ruleid: test-hof-callback-taint
    return sink(app_with_direct_flow(sanitizes, source()));
}

function test_direct_flow_sanitizing_lambda() {
    // ruleid: test-hof-callback-taint
    return sink(app_with_direct_flow(x => 3, source()));
}
