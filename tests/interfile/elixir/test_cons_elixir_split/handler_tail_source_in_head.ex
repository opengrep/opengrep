defmodule M do
  def handler_tail_source_in_head([_ | tail]) do
    # ok: test-cons-elixir
    sink(tail)
  end
end
