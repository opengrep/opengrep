# A module's multi-clause functions must be defined together in one
# module (one file): re-opening `defmodule TaintTest` in several files
# would redefine the module, and Elixir requires clauses of the same
# function to be grouped.  So all clauses live here; the interfile
# dimension is the cross-module caller in caller.ex.
defmodule TaintTest do
  # Multi-clause: taint flows through whichever clause matches at runtime.
  def process(0) do
    sink(0)
  end
  def process(x) do
    # ruleid: multi_clause_elixir_taint
    sink(x)
  end

  # Guards: taint flows through each matching guarded clause.
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

  # Two-argument multi-clause: taint on the first arg flows through.
  def combine(0, _y) do
    sink(0)
  end
  def combine(x, _y) do
    # ruleid: multi_clause_elixir_taint
    sink(x)
  end

  # Compact do: form.
  def compact(0), do: sink(0)
  # ruleid: multi_clause_elixir_taint
  def compact(x), do: sink(x)
end
