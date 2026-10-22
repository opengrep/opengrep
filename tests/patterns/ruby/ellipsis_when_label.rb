# ERROR: match
case k
when 1
  a()
end

# ERROR: match
case k
when 1, 2
  a()
end

# ERROR: match
case k
when FOO
  a()
end
