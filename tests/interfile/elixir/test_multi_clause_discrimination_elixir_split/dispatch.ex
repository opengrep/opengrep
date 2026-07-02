defmodule DiscriminationTest do
  def dispatch(0, x) do
    # ruleid: test-multi-clause-discrimination-elixir
    sink(x)
  end
end
