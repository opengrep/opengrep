defmodule M do
  def handler_tail([_ | tail]) do
    # ruleid: test-cons-elixir
    sink(tail)
  end
end
