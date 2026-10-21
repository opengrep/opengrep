def f(a, b, n)
  if n > 0
    return f(b, "safe", n - 1)
  end
  "safe"
end

def calls_f
  # ok: ruby_recursion_rounds
  sink(f("x", source(), 3))
end

def p(a, b, n)
  if n > 0
    return q(b, "safe", n - 1)
  end
  "safe"
end

def q(a, b, n)
  p(a, b, n)
end

def calls_p
  # ok: ruby_recursion_rounds
  sink(p("x", source(), 3))
end

def keep(a, n)
  if n > 0
    return keep(a, n - 1)
  end
  a
end

def calls_keep
  # ruleid: ruby_recursion_rounds
  sink(keep(source(), 3))
end
