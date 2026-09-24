def guarded(n)
  return yield(n) if block_given?
  n
end

def guarded_else(n)
  if block_given?
    yield n
  else
    n
  end
end

def unguarded(n)
  yield n
end

def go
  # ruleid: ruby_yield_without_block
  sink(guarded(source()))
  # ruleid: ruby_yield_without_block
  sink(guarded(source()) { |v| v })
  # ruleid: ruby_yield_without_block
  sink(guarded_else(source()))
  # ruleid: ruby_yield_without_block
  sink(guarded_else(source()) { |v| v })
  # ruleid: ruby_yield_without_block
  sink(unguarded(source()) { |v| v })
  # ok: ruby_yield_without_block
  sink(unguarded(source()))
  # ok: ruby_yield_without_block
  sink(guarded("safe"))
  nil
end
