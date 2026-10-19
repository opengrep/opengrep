# ERROR: match
case k
in {a: 1}
  a()
end

# ERROR: match
case k
in {b: 2, a: 1}
  a()
end

# `...` stands for any other entries, `**nil` and `**rest` included.
# ERROR: match
case k
in {a: 1, **nil}
  a()
end

# ERROR: match
case k
in {a: 1, **rest}
  a()
end

case k
in {b: 2}
  a()
end
