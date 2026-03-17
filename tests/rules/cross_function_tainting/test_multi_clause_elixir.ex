defmodule TaintTest do
  # Multi-clause function: taint flows through whichever clause is entered.
  # Clause matching literal 0 — taint still reaches sink via the second clause.
  def process(0) do
    sink(0)
  end
  def process(x) do
    # ruleid: multi_clause_elixir_taint
    sink(x)
  end

  # Guards: taint flows through the matching guarded clause.
  def transform(x) when x > 0 do
    # ruleid: multi_clause_elixir_taint
    sink(x)
  end
  def transform(x) when x < 0 do
    # ruleid: multi_clause_elixir_taint
    sink(x)
  end
  def transform(_x) do
    sink(0)
  end

  # Two-argument multi-clause: taint on first arg flows through.
  def combine(0, _y) do
    sink(0)
  end
  def combine(x, _y) do
    # ruleid: multi_clause_elixir_taint
    sink(x)
  end

  # Compact do: form — multi-clause with short syntax.
  def compact(0), do: sink(0)
  # ruleid: multi_clause_elixir_taint
  def compact(x), do: sink(x)

  def main() do
    process(source())
    transform(source())
    combine(source(), 42)
    compact(source())
  end
end
