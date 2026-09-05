def cond
  false
end

def p(x)
  q(x)
end

def q(x)
  r(source())
end

def r(x)
  return p(x) if cond
  x
end
