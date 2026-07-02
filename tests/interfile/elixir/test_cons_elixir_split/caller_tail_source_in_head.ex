defmodule M do
  def caller_tail_source_in_head() do
    # source goes to head; tail covers positions [1..]
    handler_tail_source_in_head([source(), "ok"])
  end
end
