# Comprehensive HOF test for Python: Custom higher-order functions
# All of these should detect taint flow from source() to sink()

# ===== Custom HOF Functions =====

# Manual loop implementation
# Delegates to built-in (tests ToSinkInCall propagation)
# ===== Test Cases =====

# Test custom HOF with manual loop + lambda
# Test custom HOF with manual loop + named function
# Test custom HOF delegating to built-in + lambda
# Test custom HOF delegating to built-in + named function
# ===== Built-in methods (if supported) =====

# ===== Complex Example =====

# ===== Top-level HOF Tests =====
# These test HOF callback detection at module level (outside any function)

# Top-level lambda callback
# ruleid: test-hof-taint
toplevel_sink = lambda x: sink(x)
toplevel_sink(source())

# Top-level function HOF (map with named callback)
toplevel_items = [source()]
list(map(toplevel_handler, toplevel_items))

# Top-level user-defined HOF
custom_for_each(toplevel_items, toplevel_handler)
