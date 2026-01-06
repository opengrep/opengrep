# Test: Ruby collection models for taint propagation

def test_array_push(tainted)
  arr = []
  arr.push(tainted)
  # ruleid: ruby-collection-taint
  sink(arr)
end

def test_array_pop(tainted)
  arr = [tainted]
  value = arr.pop
  # ruleid: ruby-collection-taint
  sink(value)
end

def test_array_shift(tainted)
  arr = [tainted]
  value = arr.shift
  # ruleid: ruby-collection-taint
  sink(value)
end

def test_hash_fetch(tainted)
  h = { "key" => tainted }
  value = h.fetch("key")
  # ruleid: ruby-collection-taint
  sink(value)
end

def test_array_first(tainted)
  arr = [tainted]
  value = arr.first
  # ruleid: ruby-collection-taint
  sink(value)
end

def sink(data)
  puts data
end
