# ERROR: match
case k
in {}
  a()
end

# An empty hash pattern is not an empty array pattern.
case k
in []
  a()
end

# It matches only an empty hash, so it does not match a pattern with keys.
case k
in {a: 1}
  a()
end
