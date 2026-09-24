def each_item(n)
  yield n
end

def wrap(n)
  x = yield n
  x
end

def run_block(n)
  yield
end

def go
  r = each_item(source()) { |v| v }
  # ruleid: ruby_yield_value
  sink(r)
  s = wrap(source()) { |v| v }
  # ruleid: ruby_yield_value
  sink(s)
  t = run_block(source()) { 1 }
  # ok: ruby_yield_value
  sink(t)
end
