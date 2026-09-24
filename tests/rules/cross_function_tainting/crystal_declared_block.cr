def with_block(a, &block)
  yield a
end

def with_rest_and_block(a, *xs, &block)
  yield xs
end

def go
  with_block(source()) do |v|
    # ruleid: crystal_declared_block
    sink(v)
  end

  with_rest_and_block("safe", "safe2", source()) do |v|
    # ruleid: crystal_declared_block
    sink(v)
  end

  with_block("safe") do |v|
    # ok: crystal_declared_block
    sink(v)
  end
  nil
end
