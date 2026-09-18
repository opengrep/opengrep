# Comprehensive HOF test for Julia: Custom and built-in higher-order functions
# All of these should detect taint flow from source() to sink()

# ===== Custom HOF Functions =====

# Manual loop implementation

# Delegates to built-in (tests ToSinkInCall propagation)



# ===== Test Cases =====

# Named function for testing

# Test custom HOF with manual loop + lambda

# Test custom HOF with manual loop + named function

# Test custom HOF delegating to built-in + lambda

# Test custom HOF delegating to built-in + named function



# ===== Built-in functions =====




# ===== Complex Example =====



# Stub functions


# ===== Top-level HOF Tests =====
# These test HOF callback detection at module level (outside any function)

# Top-level lambda callback
# ruleid: test-hof-taint
toplevel_sink = x -> sink(x)
toplevel_sink(source())

# Top-level function HOF (map with named callback)

toplevel_items = [source()]
map(toplevel_handler, toplevel_items)

# Top-level user-defined HOF
customForEach(toplevel_items, toplevel_handler)
