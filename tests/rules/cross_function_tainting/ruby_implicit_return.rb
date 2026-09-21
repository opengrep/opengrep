def interpolated(n)
  "a #{n}"
end

def ternary(n)
  n ? n : "b"
end

def ternary_interpolated(n)
  n ? "a #{n}" : "b"
end

def or_default(n)
  n || "b"
end

def if_interpolated(n)
  if n
    "a #{n}"
  else
    "b"
  end
end

def concatenated(n)
  "a " + n
end

def constant(n)
  "a"
end

def go
  # ruleid: ruby_implicit_return
  sink(interpolated(source()))
  # ruleid: ruby_implicit_return
  sink(ternary(source()))
  # ruleid: ruby_implicit_return
  sink(ternary_interpolated(source()))
  # ruleid: ruby_implicit_return
  sink(or_default(source()))
  # ruleid: ruby_implicit_return
  sink(if_interpolated(source()))
  # ruleid: ruby_implicit_return
  sink(concatenated(source()))
  # ok: ruby_implicit_return
  sink(constant(source()))
end
