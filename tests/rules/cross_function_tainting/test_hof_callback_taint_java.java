// Test HOF taint propagation based on callback behavior.
// Tests both callback-only flow and callback + direct flow patterns.

import java.util.function.Function;

public class TestHofCallbackTaint {

    static String propagates(String x) {
        return x;
    }

    static String sanitizes(String x) {
        return "3";
    }

    // HOF where taint flows only through callback return
    static String app_callback_only(Function<String, String> f, String x) {
        return f.apply(x);
    }

    // HOF where taint flows through callback AND directly via x
    static String app_with_direct_flow(Function<String, String> f, String x) {
        return f.apply(x) + x;
    }

    // === Callback-only HOF tests ===

    void test_callback_only_propagating_named() {
        // todoruleid: test-hof-callback-taint
        // TODO: Java method references (Class::method) not yet supported in IL
        sink(app_callback_only(TestHofCallbackTaint::propagates, source()));
    }

    void test_callback_only_propagating_lambda() {
        // ruleid: test-hof-callback-taint
        sink(app_callback_only(x -> x, source()));
    }

    // NOTE: Method references (Class::method) not yet supported in IL
    // This test would pass for wrong reason - skipping until method refs work
    // void test_callback_only_sanitizing_named() {
    //     // ok: test-hof-callback-taint
    //     sink(app_callback_only(TestHofCallbackTaint::sanitizes, source()));
    // }

    void test_callback_only_sanitizing_lambda() {
        // ok: test-hof-callback-taint
        sink(app_callback_only(x -> "3", source()));
    }

    // === Direct flow HOF tests (taint always flows via + x) ===

    void test_direct_flow_propagating_named() {
        // ruleid: test-hof-callback-taint
        sink(app_with_direct_flow(TestHofCallbackTaint::propagates, source()));
    }

    void test_direct_flow_propagating_lambda() {
        // ruleid: test-hof-callback-taint
        sink(app_with_direct_flow(x -> x, source()));
    }

    void test_direct_flow_sanitizing_named() {
        // ruleid: test-hof-callback-taint
        sink(app_with_direct_flow(TestHofCallbackTaint::sanitizes, source()));
    }

    void test_direct_flow_sanitizing_lambda() {
        // ruleid: test-hof-callback-taint
        sink(app_with_direct_flow(x -> "3", source()));
    }

    static String source() { return "tainted"; }
    static void sink(String x) { }
}
