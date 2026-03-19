# Test HOF taint propagation based on callback behavior.
# Tests both callback-only flow and callback + direct flow patterns.

def propagates(x)
  x
end

def sanitizes(x)
  "3"
end

# HOF where taint flows only through callback return
def app_callback_only(f, x)
  f.call(x)
end

# HOF where taint flows through callback AND directly via x
def app_with_direct_flow(f, x)
  f.call(x) + x
end

# === Callback-only HOF tests ===

def test_callback_only_propagating_lambda()
  # ruleid: test-hof-callback-taint
  sink(app_callback_only(->(x) { x }, source()))
end

# NOTE: Ruby lambda callbacks not yet working
# This test would pass for wrong reason - skipping until callbacks work
# def test_callback_only_sanitizing_lambda()
#   # ok: test-hof-callback-taint
#   sink(app_callback_only(->(x) { "3" }, source()))
# end

# === Direct flow HOF tests (taint always flows via + x) ===

def test_direct_flow_propagating_lambda()
  # ruleid: test-hof-callback-taint
  sink(app_with_direct_flow(->(x) { x }, source()))
end

def test_direct_flow_sanitizing_lambda()
  # ruleid: test-hof-callback-taint
  sink(app_with_direct_flow(->(x) { "3" }, source()))
end

def source()
  "tainted"
end

def sink(x)
end
