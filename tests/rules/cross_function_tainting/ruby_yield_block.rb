def with_yield(n)
  yield n
end

def with_block_and_yield(n, &block)
  yield n
end

def with_yield_in_each(items)
  items.each { |i| yield i }
end

def with_yield_in_for(items)
  for i in items
    yield i
  end
end

def go
  with_yield_in_for([source()]) do |v|
    # ruleid: ruby_yield_block
    sink(v)
  end

  with_yield(source()) do |v|
    # ruleid: ruby_yield_block
    sink(v)
  end

  with_yield("safe") do |v|
    # ok: ruby_yield_block
    sink(v)
  end

  with_block_and_yield(source()) do |v|
    # ruleid: ruby_yield_block
    sink(v)
  end

  with_yield_in_each([source()]) do |v|
    # ruleid: ruby_yield_block
    sink(v)
  end

  with_yield_in_each(["safe"]) do |v|
    # ok: ruby_yield_block
    sink(v)
  end

  with_yield(source()) { |v| other(v) }
  # ok: ruby_yield_block
  sink("safe")
end
