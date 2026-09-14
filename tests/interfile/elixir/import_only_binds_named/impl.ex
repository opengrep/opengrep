defmodule App.Impl do
  def sink(x), do: IO.puts(x)

  def greet(msg) do
    # ruleid: test-import-only-binds-named
    sink(msg)
  end

  def other(msg) do
    # ok: test-import-only-binds-named
    sink(msg)
  end
end
