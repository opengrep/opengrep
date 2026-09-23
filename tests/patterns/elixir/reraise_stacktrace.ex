defmodule ReraiseStacktracePattern do
  def with_builtin(err) do
    try do
      dangerous()
    rescue
      _e ->
        # ERROR:
        reraise err, __STACKTRACE__
    end
  end

  def with_bound(err, custom_st) do
    reraise err, custom_st
  end
end
