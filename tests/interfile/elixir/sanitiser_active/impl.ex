defmodule App.Impl do
  def sink(x), do: IO.puts(x)

  def emit(data) do
    # ok: test-sanitiser
    sink(data)
  end
end
