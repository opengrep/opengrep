# Multi-clause Elixir lambda HOF test
# Tests per-arity signature extraction for multi-clause fn lambdas

defmodule TestMultiClause do
  # Custom HOF that calls its callback with a value
  def direct_call(callback, value) do
    callback.(value)
  end

  # Test 1: Multi-clause fn lambda used as HOF callback
  # The 2-arity clause sinks the first arg
  def test_multi_clause_lambda() do
    handler = fn
      x, nil ->
        # ruleid: test-hof-taint
        sink(x)
      x, y ->
        # ruleid: test-hof-taint
        sink(y)
    end
    direct_call(handler, source())
  end

  # Test 2: Named function as callback with single clause
  def process(x) do
    # ruleid: test-hof-taint
    sink(x)
  end

  def test_named_callback() do
    direct_call(&process/1, source())
  end

  # Test 3: Single-clause fn lambda
  def test_single_clause_lambda() do
    # ruleid: test-hof-taint
    direct_call(fn x -> sink(x) end, source())
  end

  # Test 4: Multi-clause fn lambda with guard (PatWhen)
  def test_guarded_clause() do
    handler = fn
      x when is_binary(x) ->
        # ruleid: test-hof-taint
        sink(x)
      x ->
        x
    end
    direct_call(handler, source())
  end
end
