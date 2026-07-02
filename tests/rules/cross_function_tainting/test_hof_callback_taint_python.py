# Test HOF taint propagation based on callback behavior.
# Tests both callback-only flow and callback + direct flow patterns.

def propagates(x):
    return x

def sanitizes(x):
    return 3

# HOF where taint flows only through callback return
def app_callback_only(f, x):
    return f(x)

# HOF where taint flows through callback AND directly via x
def app_with_direct_flow(f, x):
    return f(x) + x

# === Callback-only HOF tests ===

def test_callback_only_propagating_named():
    # ruleid: test-hof-callback-taint
    return sink(app_callback_only(propagates, source()))

def test_callback_only_propagating_lambda():
    # ruleid: test-hof-callback-taint
    return sink(app_callback_only(lambda x: x, source()))

def test_callback_only_sanitizing_named():
    # ok: test-hof-callback-taint
    return sink(app_callback_only(sanitizes, source()))

def test_callback_only_sanitizing_lambda():
    # ok: test-hof-callback-taint
    return sink(app_callback_only(lambda x: 3, source()))

# === Direct flow HOF tests (taint always flows via + x) ===

def test_direct_flow_propagating_named():
    # ruleid: test-hof-callback-taint
    return sink(app_with_direct_flow(propagates, source()))

def test_direct_flow_propagating_lambda():
    # ruleid: test-hof-callback-taint
    return sink(app_with_direct_flow(lambda x: x, source()))

def test_direct_flow_sanitizing_named():
    # ruleid: test-hof-callback-taint
    return sink(app_with_direct_flow(sanitizes, source()))

def test_direct_flow_sanitizing_lambda():
    # ruleid: test-hof-callback-taint
    return sink(app_with_direct_flow(lambda x: 3, source()))

# === ToSinkInCall propagation tests ===
# When a HOF's callback parameter is itself a parameter of the
# enclosing function, ToSinkInCall should propagate upward so
# the finding is detected at the outer call site.

def wrapper_propagates_callback(f, x):
    return app_callback_only(f, x)

def test_tosinkincall_propagated():
    # ruleid: test-hof-callback-taint
    return sink(wrapper_propagates_callback(propagates, source()))

def test_tosinkincall_propagated_sanitized():
    # ok: test-hof-callback-taint
    return sink(wrapper_propagates_callback(sanitizes, source()))

# === Wrong arg index regression test ===
# When a HOF is called with a concrete (non-parameter) callback whose
# signature is NOT available, the old code preserved ToSinkInCall with
# the inner function's arg index.  This aliases it to a wrong parameter
# of the enclosing function, causing a false positive.

def wrapper_ignores_callback(callback, data):
    # `unknown_sanitizer` is NOT defined — no signature available.
    # The HOF's ToSinkInCall for arg 0 should be DROPPED, not
    # preserved with index=0 (which would alias to `callback`).
    return app_callback_only(unknown_sanitizer, data)

def test_wrong_arg_index_no_fp():
    # ok: test-hof-callback-taint
    # Without fix: ToSinkInCall preserved with arg index 0 → resolves
    # `propagates` as callback → FP.  With fix: dropped → correct.
    return sink(wrapper_ignores_callback(propagates, source()))

# === All-resolved ToSink dedup test ===
# When a function contains source()→sink() entirely within its body,
# the finding is reported at that function.  The signature should NOT
# carry a ToSink effect with all-Src taints, because callers would
# re-report the same finding as a duplicate.

def direct_source_to_sink():
    # ruleid: test-hof-callback-taint
    sink(source())

def caller_no_dup_a():
    # ok: test-hof-callback-taint
    direct_source_to_sink()

def caller_no_dup_b():
    # ok: test-hof-callback-taint
    caller_no_dup_a()

def caller_no_dup_c():
    # ok: test-hof-callback-taint
    caller_no_dup_b()
