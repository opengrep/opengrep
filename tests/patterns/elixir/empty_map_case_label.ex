# ERROR: match
case m do
  %{} -> 1
end

# A rule `%{}` matches the empty map pattern only.
case m do
  %{a: 1} -> 1
end

# An empty map is not an empty list.
case m do
  [] -> 1
end
