# ERROR: match
case m do
  %{} -> 1
end

# ERROR: match
case m do
  %{a: 1} -> 1
end

# A map pattern is not a list pattern.
case m do
  [] -> 1
end
