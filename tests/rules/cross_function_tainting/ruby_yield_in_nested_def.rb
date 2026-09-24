def outer_rest(*xs)
  def inner_a
    yield 1
  end
  # ruleid: ruby_yield_in_nested_def
  sink(xs)
end

def outer_pos(a, b)
  def inner_b
    yield 1
  end
  # ruleid: ruby_yield_in_nested_def
  sink(b)
end

def outer_safe(*xs)
  def inner_c
    yield 1
  end
  # ok: ruby_yield_in_nested_def
  sink("safe")
end

def go
  outer_rest(source())
  outer_pos("safe", source())
  outer_safe(source())
  inner_a do |v|
    # ok: ruby_yield_in_nested_def
    sink(v)
  end
end
