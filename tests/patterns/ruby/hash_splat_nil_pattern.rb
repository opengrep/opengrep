# ERROR: match
case k
in {a: 1, **nil}
  a()
end

# `**nil` is not a rest binding.
case k
in {a: 1, **rest}
  a()
end

# Without `**nil`, other keys are allowed.
case k
in {a: 1}
  a()
end
