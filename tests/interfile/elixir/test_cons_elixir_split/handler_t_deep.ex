defmodule M do
  def handler_t_deep([_a, _b | t]) do
    # ruleid: test-cons-elixir
    sink(t)
  end
end
