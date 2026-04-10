defmodule Taint.MapValue do
  # taint flows through arrow map value expression
  def transform_arrow(items) do
    items
    # ruleid: taint-map-value
    |> Enum.map(fn {k, v} -> %{sink(k) => v} end)
  end

  # taint flows through complex key expression in arrow map
  def transform_arrow_key(items) do
    items
    # ruleid: taint-map-value
    |> Enum.map(fn {k, v} -> %{String.downcase(sink(k)) => v} end)
  end

  # taint flows through keyword map value expression
  def transform_keyword(items) do
    items
    # ruleid: taint-map-value
    |> Enum.map(fn {k, v} -> %{result: sink(k)} end)
  end

  # taint through a direct call, not inside a map
  def transform_direct(items) do
    items
    # ruleid: taint-map-value
    |> Enum.map(fn {k, v} -> sink(k) end)
  end

  # safe: variable not from param
  def no_taint(items) do
    items
    |> Enum.map(fn {_k, _v} ->
      other = "safe"
      # ok: taint-map-value
      sink(other)
    end)
  end
end
