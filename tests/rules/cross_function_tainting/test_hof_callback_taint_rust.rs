// Test HOF taint propagation based on callback behavior.
// Tests both callback-only flow and callback + direct flow patterns.

fn propagates(x: String) -> String {
    x
}

fn sanitizes(_x: String) -> String {
    "3".to_string()
}

// HOF where taint flows only through callback return
fn app_callback_only<F>(f: F, x: String) -> String
where
    F: Fn(String) -> String,
{
    f(x)
}

// HOF where taint flows through callback AND directly via x
fn app_with_direct_flow<F>(f: F, x: String) -> String
where
    F: Fn(String) -> String,
{
    f(x.clone()) + &x
}

// === Callback-only HOF tests ===

fn test_callback_only_propagating_named() {
    // ruleid: test-hof-callback-taint
    sink(app_callback_only(propagates, source()));
}

fn test_callback_only_propagating_lambda() {
    // ruleid: test-hof-callback-taint
    sink(app_callback_only(|x| x, source()));
}

fn test_callback_only_sanitizing_named() {
    // ok: test-hof-callback-taint
    sink(app_callback_only(sanitizes, source()));
}

fn test_callback_only_sanitizing_lambda() {
    // ok: test-hof-callback-taint
    sink(app_callback_only(|_x| "3".to_string(), source()));
}

// === Direct flow HOF tests (taint always flows via + x) ===

fn test_direct_flow_propagating_named() {
    // ruleid: test-hof-callback-taint
    sink(app_with_direct_flow(propagates, source()));
}

fn test_direct_flow_propagating_lambda() {
    // ruleid: test-hof-callback-taint
    sink(app_with_direct_flow(|x| x, source()));
}

fn test_direct_flow_sanitizing_named() {
    // ruleid: test-hof-callback-taint
    sink(app_with_direct_flow(sanitizes, source()));
}

fn test_direct_flow_sanitizing_lambda() {
    // ruleid: test-hof-callback-taint
    sink(app_with_direct_flow(|_x| "3".to_string(), source()));
}

fn source() -> String { "tainted".to_string() }
fn sink(_x: String) {}
