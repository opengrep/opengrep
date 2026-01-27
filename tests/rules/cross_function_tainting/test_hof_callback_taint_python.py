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
