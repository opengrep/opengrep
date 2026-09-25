# The function apply/3 calls the function its atom gives in the module its
# first argument gives, so it reaches that module's function only.
defmodule Handler do
  def f(x) do
    # ruleid: regression_remote_call_elixir
    sink(x)
  end
end

defmodule Other do
  def f(x) do
    # ok: regression_remote_call_elixir
    sink(x)
  end
end

defmodule Main do
  def run do
    Handler.f(source())
  end
end
