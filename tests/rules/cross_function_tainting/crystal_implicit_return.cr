def interpolated(n)
  "a #{n}"
end

def constant(n)
  "a"
end

def go
  # ruleid: crystal_implicit_return
  sink(interpolated(source()))
  # ok: crystal_implicit_return
  sink(constant(source()))
  nil
end
