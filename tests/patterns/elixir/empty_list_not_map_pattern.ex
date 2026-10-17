# An empty map pattern is not an empty list.
case m do
  %{} -> 1
end

case m do
  # ERROR: match
  [] -> 1
end
