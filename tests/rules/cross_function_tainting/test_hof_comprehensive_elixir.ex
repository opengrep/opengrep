# Comprehensive HOF test for Elixir: Custom and built-in higher-order functions
# Tests all three lambda syntaxes: named references, fn lambdas, and shorthand lambdas
# All of these should detect taint flow from source() to sink()

defmodule TestHOF do
  # ===== Custom HOF Functions =====

  # Delegates to built-in (tests ToSinkInCall propagation)
  def custom_map_builtin(arr, callback) do
    Enum.map(arr, callback)
  end

  def custom_for_each(arr, callback) do
    Enum.each(arr, callback)
  end

  def custom_filter(arr, callback) do
    Enum.filter(arr, callback)
  end

  def direct_call(callback) do
    callback.(source())
  end

  # ===== Named function reference tests =====

  def process(x) do
    # ruleid: test-hof-taint
    sink(x)
    x
  end

  # Test custom HOF delegating to built-in + named function
  def test_custom_map_builtin_named() do
    arr = [source()]
    mapped = custom_map_builtin(arr, &process/1)
  end

  def test_builtin_map_named() do
    arr = [source()]
    mapped = Enum.map(arr, &process/1)
  end

  def test_custom_foreach_named() do
    arr = [source()]
    mapped = custom_for_each(arr, &process/1)
  end

  def test_builtin_each_named() do
    arr = [source()]
    mapped = Enum.each(arr, &process/1)
  end

  def process_filter(x) do
    # ruleid: test-hof-taint
    sink(x)
    true
  end

  def test_custom_filter_named() do
    arr = [source()]
    filtered = custom_filter(arr, &process_filter/1)
  end

  def test_builtin_filter_named() do
    arr = [source()]
    filtered = Enum.filter(arr, &process_filter/1)
  end

  def test_direct_call_named() do
    result = direct_call(&process/1)
  end

  # ===== Regular fn lambda tests =====

  def test_custom_map_builtin_fn_lambda() do
    arr = [source()]
    mapped = custom_map_builtin(arr, fn x ->
      # ruleid: test-hof-taint
      sink(x)
      x
    end)
  end

  def test_builtin_map_fn_lambda() do
    arr = [source()]
    mapped = Enum.map(arr, fn x ->
      # ruleid: test-hof-taint
      sink(x)
      x
    end)
  end

  def test_custom_foreach_fn_lambda() do
    arr = [source()]
    mapped = custom_for_each(arr, fn x ->
      # ruleid: test-hof-taint
      sink(x)
    end)
  end

  def test_builtin_each_fn_lambda() do
    arr = [source()]
    mapped = Enum.each(arr, fn x ->
      # ruleid: test-hof-taint
      sink(x)
    end)
  end

  def test_custom_filter_fn_lambda() do
    arr = [source()]
    filtered = custom_filter(arr, fn x ->
      # ruleid: test-hof-taint
      sink(x)
      true
    end)
  end

  def test_builtin_filter_fn_lambda() do
    arr = [source()]
    filtered = Enum.filter(arr, fn x ->
      # ruleid: test-hof-taint
      sink(x)
      true
    end)
  end

  def test_direct_call_fn_lambda() do
    result = direct_call(fn x ->
      # ruleid: test-hof-taint
      sink(x)
      x
    end)
  end

  # ===== Shorthand lambda tests =====

  def test_custom_map_builtin_shorthand() do
    arr = [source()]
    # ruleid: test-hof-taint
    mapped = custom_map_builtin(arr, &(sink(&1)))
  end

  def test_builtin_map_shorthand() do
    arr = [source()]
    # ruleid: test-hof-taint
    mapped = Enum.map(arr, &(sink(&1)))
  end

  def test_custom_foreach_shorthand() do
    arr = [source()]
    # ruleid: test-hof-taint
    mapped = custom_for_each(arr, &(sink(&1)))
  end

  def test_builtin_each_shorthand() do
    arr = [source()]
    # ruleid: test-hof-taint
    mapped = Enum.each(arr, &(sink(&1)))
  end

  def test_custom_filter_shorthand() do
    arr = [source()]
    # ruleid: test-hof-taint
    filtered = custom_filter(arr, &(sink(&1) || true))
  end

  def test_builtin_filter_shorthand() do
    arr = [source()]
    # ruleid: test-hof-taint
    filtered = Enum.filter(arr, &(sink(&1) || true))
  end

  # ===== Complex real-world example =====

  def get_history(name, owner) do
    result = source()
    result
  end

  def process_flat_map(node) do
    changes = node
    # ruleid: test-hof-taint
    sink(changes)
    [changes]
  end

  def test_original_example() do
    history = get_history("name", "owner")
    result = Enum.flat_map([history], &process_flat_map/1)
  end

  # Stub functions
  def source() do
    "tainted"
  end

  def sink(_s) do
    :ok
  end
end
