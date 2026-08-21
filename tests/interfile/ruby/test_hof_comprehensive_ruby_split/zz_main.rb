require_relative 'custom_map'
require_relative 'custom_for_each'
require_relative 'test_custom_map'
require_relative 'test_custom_foreach'
require_relative 'test_builtin_map'
require_relative 'test_builtin_each'
require_relative 'test_builtin_select'
require_relative 'source'
require_relative 'sink'
require_relative 'toplevel_handler'
# Comprehensive HOF test for Ruby: Custom and built-in higher-order functions
# All of these should detect taint flow from source() to sink()

# ===== Custom HOF Functions =====



# ===== Test Cases =====



# ===== Built-in methods =====




# Stub methods


# ===== Top-level HOF Tests =====
# These test HOF callback detection at top level (outside any def)

# Top-level lambda callback
# ruleid: test-hof-taint
toplevel_sink = ->(x) { sink(x) }
toplevel_sink.(source())

# ruleid: test-hof-taint
toplevel_sink1 = ->(x) { sink(x) }
# ruleid: test-hof-taint
toplevel_sink2 = ->(x) { sink(x) }

# Top-level method HOF (each with block)
toplevel_items = [source()]
toplevel_items.each(&toplevel_sink1)

# Named callback for top-level HOF
toplevel_handler(source())
# Top-level user-defined HOF
custom_for_each(toplevel_items, &toplevel_sink2)
