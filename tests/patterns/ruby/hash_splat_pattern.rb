# ERROR: match
case k
in {a: 1, **rest}
  a()
end

# `**nil` binds nothing.
case k
in {a: 1, **nil}
  a()
end
