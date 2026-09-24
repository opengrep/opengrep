def with_yield(n)
  yield n
end

def go
  with_yield(source()) do |v|
    # ruleid: crystal_yield
    sink(v)
  end

  with_yield("safe") do |v|
    # ok: crystal_yield
    sink(v)
  end

  r = with_yield(source()) { |v| v }
  # ruleid: crystal_yield
  sink(r)
  nil
end
