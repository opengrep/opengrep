# Test HOF taint propagation based on callback behavior.
# Tests both callback-only flow and callback + direct flow patterns.

# HOF where taint flows only through callback return
# HOF where taint flows through callback AND directly via x
# === Callback-only HOF tests ===

# === Direct flow HOF tests (taint always flows via + x) ===

# === ToSinkInCall propagation tests ===
# When a HOF's callback parameter is itself a parameter of the
# enclosing function, ToSinkInCall should propagate upward so
# the finding is detected at the outer call site.

# === Wrong arg index regression test ===
# When a HOF is called with a concrete (non-parameter) callback whose
# signature is NOT available, the old code preserved ToSinkInCall with
# the inner function's arg index.  This aliases it to a wrong parameter
# of the enclosing function, causing a false positive.

# === All-resolved ToSink dedup test ===
# When a function contains source()→sink() entirely within its body,
# the finding is reported at that function.  The signature should NOT
# carry a ToSink effect with all-Src taints, because callers would
# re-report the same finding as a duplicate.

