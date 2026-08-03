def query
  1
end

def tuple_target
  # Multiple assignment declares locals: `query` must NOT resolve to the
  # top-level `def query`.
  query, rest = make(), 0
  sink(query)
end

# Ruby local variables and methods live in separate namespaces, so a
# top-level assignment declares a local; it must not resolve to the def.
query = make()
sink(query)
