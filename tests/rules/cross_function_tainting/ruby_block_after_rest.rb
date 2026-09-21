def with_yield(a, *xs)
  yield a
end

def with_block(a, *xs, &block)
  block.call(a)
end

def last_positional(a, *xs, b)
  b
end

def go
  with_yield(source(), 1, 2) do |v|
    # ruleid: ruby_block_after_rest
    sink(v)
  end

  with_block(source(), 1, 2) do |v|
    # ruleid: ruby_block_after_rest
    sink(v)
  end

  with_yield(source()) do |v|
    # ruleid: ruby_block_after_rest
    sink(v)
  end

  with_yield("safe", source()) do |v|
    # ok: ruby_block_after_rest
    sink(v)
  end

  # ruleid: ruby_block_after_rest
  sink(last_positional(1, 2, source()))
  # ok: ruby_block_after_rest
  sink(last_positional(source(), 2, 3))
  nil
end
