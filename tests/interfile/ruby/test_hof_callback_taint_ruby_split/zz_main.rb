require_relative 'propagates'
require_relative 'sanitizes'
require_relative 'app_callback_only'
require_relative 'app_with_direct_flow'
require_relative 'test_callback_only_propagating_lambda'
require_relative 'test_direct_flow_propagating_lambda'
require_relative 'test_direct_flow_sanitizing_lambda'
require_relative 'source'
require_relative 'sink'
# Test HOF taint propagation based on callback behavior.
# Tests both callback-only flow and callback + direct flow patterns.



# HOF where taint flows only through callback return

# HOF where taint flows through callback AND directly via x

# === Callback-only HOF tests ===


# NOTE: Ruby lambda callbacks not yet working
# This test would pass for wrong reason - skipping until callbacks work
# def test_callback_only_sanitizing_lambda()
#   # ok: test-hof-callback-taint
#   sink(app_callback_only(->(x) { "3" }, source()))
# end

# === Direct flow HOF tests (taint always flows via + x) ===




