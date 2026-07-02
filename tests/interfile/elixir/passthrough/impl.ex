defmodule App.Impl do
  def sink(x), do: IO.puts(x)

  def leak(x) do
    # ruleid: test-passthrough
    sink(x)
  end
end
