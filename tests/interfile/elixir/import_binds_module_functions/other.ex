defmodule Other.Impl do
  def sink(x), do: IO.puts(x)

  def greet(msg) do
    # ok: test-import-binds-module-functions
    sink(msg)
  end
end
