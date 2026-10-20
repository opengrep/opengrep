# ERROR: match
case m do
  %{} -> 1
end

# A rule `%{}` matches any map pattern: a map pattern matches a map with
# other entries too.
# ERROR: match
case m do
  %{a: 1} -> 1
end

# An empty map is not an empty list.
case m do
  [] -> 1
end
