defmodule Other.Impl do
  def sink(x), do: IO.puts(x)

  def greet(msg) do
    # ok: test-alias-as
    sink(msg)
  end
end
