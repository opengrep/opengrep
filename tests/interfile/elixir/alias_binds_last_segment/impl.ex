defmodule App.Impl do
  def sink(x), do: IO.puts(x)

  def greet(msg) do
    # ruleid: test-alias-binds-last-segment
    sink(msg)
  end
end
