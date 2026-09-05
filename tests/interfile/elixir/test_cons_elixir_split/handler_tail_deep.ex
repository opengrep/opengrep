defmodule M do
  def handler_tail_deep([_ | tail]) do
    # ruleid: test-cons-elixir
    sink(tail)
  end
end
