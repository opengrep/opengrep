// Test HOF taint propagation based on callback behavior.
// Tests both callback-only flow and callback + direct flow patterns.

function propagates(x: string): string {
    return x;
}

function sanitizes(x: string): number {
    return 3;
}

// HOF where taint flows only through callback return
function app_callback_only<T, R>(f: (x: T) => R, x: T): R {
    return f(x);
}

// HOF where taint flows through callback AND directly via x
function app_with_direct_flow(f: (x: string) => string, x: string): string {
    return f(x) + x;
}

// === Callback-only HOF tests ===

function test_callback_only_propagating_named(): void {
    // ruleid: test-hof-callback-taint
    sink(app_callback_only(propagates, source()));
}

function test_callback_only_propagating_lambda(): void {
    // ruleid: test-hof-callback-taint
    sink(app_callback_only((x: string) => x, source()));
}

function test_callback_only_sanitizing_named(): void {
    // ok: test-hof-callback-taint
    sink(app_callback_only(sanitizes, source()));
}

function test_callback_only_sanitizing_lambda(): void {
    // ok: test-hof-callback-taint
    sink(app_callback_only((x: string) => 3, source()));
}

// === Direct flow HOF tests (taint always flows via + x) ===

function test_direct_flow_propagating_named(): void {
    // ruleid: test-hof-callback-taint
    sink(app_with_direct_flow(propagates, source()));
}

function test_direct_flow_propagating_lambda(): void {
    // ruleid: test-hof-callback-taint
    sink(app_with_direct_flow((x: string) => x, source()));
}

function test_direct_flow_sanitizing_named(): void {
    // ruleid: test-hof-callback-taint
    sink(app_with_direct_flow((x: string) => "safe", source()));
}

function test_direct_flow_sanitizing_lambda(): void {
    // ruleid: test-hof-callback-taint
    sink(app_with_direct_flow((x: string) => "safe", source()));
}

declare function source(): string;
declare function sink(x: any): void;
