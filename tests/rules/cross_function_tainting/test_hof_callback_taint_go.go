package main

// Test HOF taint propagation based on callback behavior.
// Tests both callback-only flow and callback + direct flow patterns.

func propagates(x string) string {
	return x
}

func sanitizes(x string) string {
	return "3"
}

// HOF where taint flows only through callback return
func app_callback_only(f func(string) string, x string) string {
	return f(x)
}

// HOF where taint flows through callback AND directly via x
func app_with_direct_flow(f func(string) string, x string) string {
	return f(x) + x
}

// === Callback-only HOF tests ===

func test_callback_only_propagating_named() {
	// ruleid: test-hof-callback-taint
	sink(app_callback_only(propagates, source()))
}

func test_callback_only_propagating_lambda() {
	// ruleid: test-hof-callback-taint
	sink(app_callback_only(func(x string) string { return x }, source()))
}

func test_callback_only_sanitizing_named() {
	// ok: test-hof-callback-taint
	sink(app_callback_only(sanitizes, source()))
}

func test_callback_only_sanitizing_lambda() {
	// ok: test-hof-callback-taint
	sink(app_callback_only(func(x string) string { return "3" }, source()))
}

// === Direct flow HOF tests (taint always flows via + x) ===

func test_direct_flow_propagating_named() {
	// ruleid: test-hof-callback-taint
	sink(app_with_direct_flow(propagates, source()))
}

func test_direct_flow_propagating_lambda() {
	// ruleid: test-hof-callback-taint
	sink(app_with_direct_flow(func(x string) string { return x }, source()))
}

func test_direct_flow_sanitizing_named() {
	// ruleid: test-hof-callback-taint
	sink(app_with_direct_flow(sanitizes, source()))
}

func test_direct_flow_sanitizing_lambda() {
	// ruleid: test-hof-callback-taint
	sink(app_with_direct_flow(func(x string) string { return "3" }, source()))
}

func source() string { return "tainted" }
func sink(x string) {}
