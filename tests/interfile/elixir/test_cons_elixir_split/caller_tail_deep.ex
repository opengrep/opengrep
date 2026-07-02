defmodule M do
  def caller_tail_deep() do
    # source four positions into the tail range
    handler_tail_deep(["safe", "a", "b", "c", "d", source()])
  end
end
