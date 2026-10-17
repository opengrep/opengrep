# ERROR: match
case k
in ::BAR
  a()
end

# A constant without the top scope is another name.
case k
in BAR
  a()
end
