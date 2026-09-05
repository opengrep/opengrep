defmodule M do
  def caller_tail() do
    handler_tail(["safe", source()])
  end
end
