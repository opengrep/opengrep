def m
  x = yield source()
  # ruleid: yield_value
  sink(x)
  y = yield
  # ok: yield_value
  sink(y)
end
