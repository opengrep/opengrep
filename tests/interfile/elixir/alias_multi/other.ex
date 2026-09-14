defmodule Other.Impl do
  def sink(x), do: IO.puts(x)

  def greet(msg) do
    # ok: test-alias-multi
    sink(msg)
  end

  def leak(msg) do
    # ok: test-alias-multi
    sink(msg)
  end
end

defmodule Other.Mid do
  def pass(m) do
    Other.Impl.leak(m)
  end
end
