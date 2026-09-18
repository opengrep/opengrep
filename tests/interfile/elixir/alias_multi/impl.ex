defmodule App.Impl do
  def sink(x), do: IO.puts(x)

  def greet(msg) do
    # ruleid: test-alias-multi
    sink(msg)
  end

  def leak(msg) do
    # ruleid: test-alias-multi
    sink(msg)
  end
end
