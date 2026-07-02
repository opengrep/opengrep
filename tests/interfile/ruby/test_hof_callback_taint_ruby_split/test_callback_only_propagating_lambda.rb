require_relative 'propagates'
require_relative 'sanitizes'
require_relative 'app_callback_only'
require_relative 'app_with_direct_flow'
require_relative 'test_direct_flow_propagating_lambda'
require_relative 'test_direct_flow_sanitizing_lambda'
require_relative 'source'
require_relative 'sink'
require_relative 'zz_main'
def test_callback_only_propagating_lambda()
  # ruleid: test-hof-callback-taint
  sink(app_callback_only(->(x) { x }, source()))
end
