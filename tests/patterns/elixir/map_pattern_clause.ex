# ERROR: match
case k do
  %{a: 1} -> a()
end

# ERROR: match
case k do
  %{b: 2, a: 1} -> a()
end

case k do
  %{b: 2} -> a()
end
