defmodule M do
  def handler_head([head | _]) do
    # ruleid: test-cons-elixir
    sink(head)
  end
end
