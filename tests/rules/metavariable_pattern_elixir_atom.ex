defmodule M do
  # ruleid: elixir-key-id-pattern
  def m1(), do: %{body: "x"}

  # ok: elixir-key-id-pattern
  def m2(), do: %{user: "y"}
end
