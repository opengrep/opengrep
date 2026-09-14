defmodule Other.Impl do
  def sink(x), do: IO.puts(x)

  def greet(msg) do
    # ok: test-alias-binds-last-segment
    sink(msg)
  end
end
