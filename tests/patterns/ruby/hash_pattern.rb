# ERROR: match
case k
in {a: 1}
  a()
end

# A hash pattern matches a hash with other keys too.
# ERROR: match
case k
in {a: 1, b: 2}
  a()
end

# ERROR: match
case k
in {b: 2, a: 1}
  a()
end

# ERROR: match
case k
in {a: 1, **rest}
  a()
end

# `**nil`: no other keys.
case k
in {a: 1, **nil}
  a()
end
