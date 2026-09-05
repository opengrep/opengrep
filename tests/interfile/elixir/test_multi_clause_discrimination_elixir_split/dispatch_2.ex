defmodule DiscriminationTest do
  def dispatch(_n, x) do
    # ok: test-multi-clause-discrimination-elixir
    sink(x)
  end
end
