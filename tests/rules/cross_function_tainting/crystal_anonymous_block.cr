def g(a, &)
  yield a
end

def go
  g(source()) do |v|
    # ruleid: crystal_anonymous_block
    sink(v)
  end
  g("safe") do |v|
    # ok: crystal_anonymous_block
    sink(v)
  end
  nil
end
