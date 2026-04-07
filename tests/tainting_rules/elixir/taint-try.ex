defmodule TaintTry do
  # taint flows through try body
  def with_try(x) do
    try do
      # ruleid: taint-try
      sink(x)
    rescue
      ArgumentError -> :error
    end
  end

  # taint flows into the rescue handler body (x is still in scope)
  def try_rescue_handler(x) do
    try do
      some_call(x)
    rescue
      _e ->
        # ruleid: taint-try
        sink(x)
    end
  end

  # taint flows into the catch clause body (x is still in scope)
  def try_catch_handler(x) do
    try do
      some_call(x)
    catch
      _kind, _value ->
        # ruleid: taint-try
        sink(x)
    end
  end

  # taint flows into the after clause (x is still in scope)
  def try_with_after(x) do
    try do
      some_call(x)
    rescue
      _e -> :error
    after
      # ruleid: taint-try
      sink(x)
    end
  end

  # taint flows into the else clause (x is still in scope)
  def try_with_else(x) do
    try do
      some_call(x)
    else
      _result ->
        # ruleid: taint-try
        sink(x)
    end
  end

  # taint flows through body with all clauses present
  def try_with_all_clauses(x) do
    try do
      # ruleid: taint-try
      sink(x)
    rescue
      _e -> :error
    catch
      _kind, _value -> :caught
    else
      _result -> :ok
    after
      some_call(x)
    end
  end
end
