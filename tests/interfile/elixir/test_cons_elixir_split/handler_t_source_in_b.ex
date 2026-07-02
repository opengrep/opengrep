defmodule M do
  def handler_t_source_in_b([_a, _b | t]) do
    # ok: test-cons-elixir
    sink(t)
  end
end
