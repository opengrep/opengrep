# Field-sensitive taint through Elixir's Map.get / Map.fetch /
# Map.fetch! / get_in library-call idioms. A source at the bound
# key fires; a source at a sibling key does not.

def handler_get_pos(m) do
  # ruleid: test-library-access-taint
  sink(Map.get(m, :body))
end

def caller_get_pos do
  handler_get_pos(%{body: source(), user: "safe"})
end

def handler_get_neg(m) do
  # ok: test-library-access-taint
  sink(Map.get(m, :body))
end

def caller_get_neg do
  handler_get_neg(%{body: "safe", user: source()})
end

def handler_get_default(m) do
  # ruleid: test-library-access-taint
  sink(Map.get(m, :body, source()))
end

def caller_get_default do
  handler_get_default(%{body: "safe"})
end

def handler_fetch_bang(m) do
  # ruleid: test-library-access-taint
  sink(Map.fetch!(m, :body))
end

def caller_fetch_bang do
  handler_fetch_bang(%{body: source(), user: "safe"})
end

# Map.fetch returns {:ok, v} | :error. Our lowering emits both
# branches of the tuple/atom conditional so a caller case pattern
# matching {:ok, v} destructures correctly and v picks up the :body
# taint via shape projection.
def handler_fetch_ok_pos(m) do
  case Map.fetch(m, :body) do
    {:ok, v} ->
      # ruleid: test-library-access-taint
      sink(v)
    :error ->
      nil
  end
end

def caller_fetch_ok_pos do
  handler_fetch_ok_pos(%{body: source(), user: "safe"})
end

def handler_fetch_ok_neg(m) do
  case Map.fetch(m, :body) do
    {:ok, v} ->
      # ok: test-library-access-taint
      sink(v)
    :error ->
      nil
  end
end

def caller_fetch_ok_neg do
  handler_fetch_ok_neg(%{body: "safe", user: source()})
end

def handler_get_in_pos(m) do
  # ruleid: test-library-access-taint
  sink(get_in(m, [:a, :b]))
end

def caller_get_in_pos do
  handler_get_in_pos(%{a: %{b: source(), c: "safe"}})
end

def handler_get_in_neg(m) do
  # ok: test-library-access-taint
  sink(get_in(m, [:a, :b]))
end

def caller_get_in_neg do
  handler_get_in_neg(%{a: %{b: "safe", c: source()}})
end
