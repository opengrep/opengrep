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
    callback.call(item)
  end
end

# ===== Test Cases =====

def test_custom_map
  arr = [source()]
  custom_map(arr) do |x|
    # ruleid: test-hof-taint
    sink(x)
    x
  end
end

def test_custom_foreach
  arr = [source()]
  custom_for_each(arr) do |x|
    # ruleid: test-hof-taint
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
