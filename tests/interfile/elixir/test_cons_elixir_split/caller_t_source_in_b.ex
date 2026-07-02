defmodule M do
  def caller_t_source_in_b() do
    # source at position 1 binds [_b]; [t] covers positions [2..]
    handler_t_source_in_b(["safe", source(), "ok"])
  end
end
