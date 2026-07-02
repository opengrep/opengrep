defmodule M do
  def caller_t_deep() do
    handler_t_deep(["safe", "ok", "x", "y", source()])
  end
end
