# ERROR: match
case k
when g(2)
  a()
end

# ERROR: match
case k
when 2
  a()
end

# ERROR: match
case k
when 1, g(2)
  a()
end

case k
when 3
  a()
end
