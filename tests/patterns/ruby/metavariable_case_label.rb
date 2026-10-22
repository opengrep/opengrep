# ERROR: match
case k
in 1
  a
end

# ERROR: match
case k
in FOO
  a
end

# ERROR: match
case k
in x
  a
end

# ERROR: match
case k
in ^y
  a
end
