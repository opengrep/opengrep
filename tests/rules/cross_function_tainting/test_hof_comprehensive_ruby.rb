# Comprehensive HOF test for Ruby: Custom and built-in higher-order functions
# All of these should detect taint flow from source() to sink()

# ===== Custom HOF Functions =====

def custom_map(arr, &callback)
  result = []
  arr.each do |item|
    result << callback.call(item)
  end
  result
end

def custom_for_each(arr, &callback)
  arr.each do |item|
    callback(item)
  end
end

# ===== Test Cases =====

def test_custom_map
  arr = [source()]
  custom_map(arr) do |x|
    # todoruleid: test-hof-taint
    sink(x)
    x
  end
end

def test_custom_foreach
  arr = [source()]
  custom_for_each(arr) do |x|
    # todoruleid: test-hof-taint
    sink(x)
  end
end

# ===== Built-in methods =====

def test_builtin_map
  arr = [source()]
  arr.map do |x|
    # ruleid: test-hof-taint
    sink(x)
  end
end

def test_builtin_each
  arr = [source()]
  arr.each do |x|
    # ruleid: test-hof-taint
    sink(x)
  end
end

def test_builtin_select
  arr = [source()]
  arr.select do |x|
    # ruleid: test-hof-taint
    sink(x)
  end
end

# Stub methods
def source
  "tainted"
end

def sink(s)
end

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
def toplevel_handler(x)
  # ruleid: test-hof-taint
  sink(x)
end
toplevel_handler(source())
# Top-level user-defined HOF
custom_for_each(toplevel_items, &toplevel_sink2)
