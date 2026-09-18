defmodule M do
  def handler_clean_head([head | _]) do
    # ok: test-cons-elixir
    sink(head)
  end
end
