defmodule Other.Impl do
  def sink(x), do: IO.puts(x)

  def greet(msg) do
    # ok: test-import-only-binds-named
    sink(msg)
  end
end
