defmodule TaintRescue do
  # taint flows through main body even when rescue clause is present
  def with_rescue(x) do
    # ruleid: taint-rescue
    sink(x)
  rescue
    ArgumentError -> :error
  end

  # guarded function with rescue
  def guarded_rescue(x) when is_binary(x) do
    # ruleid: taint-rescue
    sink(x)
  rescue
    ArgumentError -> :error
  end

  # taint also flows into the rescue handler body (x is still in scope)
  def rescue_handler(x) do
    some_call(x)
  rescue
    _e ->
      # ruleid: taint-rescue
      sink(x)
  end

  # taint flows into the else clause (x is still in scope)
  def with_else(x) do
    some_call(x)
  else
    _result ->
      # ruleid: taint-rescue
      sink(x)
  end

  # taint flows into the after clause (x is still in scope)
  def with_after(x) do
    some_call(x)
  rescue
    _e -> :error
  after
    # ruleid: taint-rescue
    sink(x)
  end

  # taint flows through body with all clauses present
  def with_all_clauses(x) do
    # ruleid: taint-rescue
    sink(x)
  rescue
    _e -> :error
  else
    _result -> :ok
  after
    some_call(x)
  end

  # taint flows into catch clause body (x is still in scope)
  def with_catch(x) do
    some_call(x)
  catch
    _kind, _value ->
      # ruleid: taint-rescue
      sink(x)
  end
end
