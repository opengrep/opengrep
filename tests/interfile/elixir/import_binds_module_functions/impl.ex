defmodule App.Impl do
  def sink(x), do: IO.puts(x)

  def greet(msg) do
    # ruleid: test-import-binds-module-functions
    sink(msg)
  end
end
